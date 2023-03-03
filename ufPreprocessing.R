library(tidyverse)
library(fitzRoy)
library(zoo)
library(PerformanceAnalytics)
library(readxl)
library(lubridate)
source("functions.R")

data <- read.csv("fullUF2022.csv")
data <- as.tibble(data)
playerData <- as.tibble(read.csv("Playe_Data.csv"))
ids <- as.vector(read.csv("Playe_IDS.csv"))


## ---------  clean and create new data sets

df <- data %>% select(player.player.player.playerId, utcStartTime, round.roundNumber, venue.name, home.team.club.name, away.team.club.name, player.player.position, 
                      player.player.player.givenName, player.player.player.surname, timeOnGroundPercentage, goals, behinds, kicks, handballs, disposals, marks, tackles, 
                      contestedPossessions, uncontestedPossessions, inside50s, hitouts, freesFor, freesAgainst, dreamTeamPoints, rebound50s, ratingPoints, team.name)

columnNames <- c("providerId", "date", "round", "venue", "homeTeam", "awayTeam", "position", "firstName", "surname", 
                 "TOG", "goals", "behinds", "kicks", "handballs", "disposals", "marks", "tackles",
                 "contPos", "uncontPos", "i50", "hitouts", "ff", "fa", "UF", "r50", "rPts", "team")

colnames(df) <- columnNames

df <- df %>% unite(name, firstName, surname, sep = " ")

df <- df %>% filter(providerId %in% playerData$providerId)

df$date <- as.Date(df$date)
df$Season <- format(df$date, "%Y")

df <- left_join(df, playerData, by =  "providerId")

colsRemove <- c("recruitedFrom", "draftYear", "debutYear", "draftType", "draftPosition", "data_accessed", "team.y")

df <- df %>% select(-colsRemove)
df <- df %>% filter(!is.na(rPts))
df <- df %>% filter(TOG > 0)

# Function to find unique value in a ROW, used to find opponent

df$Opp <- apply(df[,c("homeTeam", "awayTeam", "team.x")], 1, funcUnique)

colsRemove2 <- c("firstName", "surname", "season", "jumperNumber", "heightInCm", "weightInKg", "homeTeam", "awayTeam")

df <- df %>% select(-colsRemove2)

df <- df %>% rename("posProvider" = "position.y", "posMatch" = "position.x", "team" = "team.x")

df$dateOfBirth <- as.Date(df$dateOfBirth)

df$Age = round(as.numeric((df$date - df$dateOfBirth) / 365.25), 1)

factorCols <- c("Season", "posProvider", "team", "posMatch", "Opp")

df[, factorCols] <- lapply(df[, factorCols], factor)

df <- df %>% mutate(KtH = kicks / handballs)
df$KtH[is.infinite(df$KtH)] <- df$kicks[is.infinite(df$KtH)]
df <- df %>% filter(!is.na(KtH))

# 2020 stat adjustment

cols2020 <- c("goals", "behinds", "kicks", "handballs", "disposals", "marks", "tackles", "contPos", "uncontPos", "i50", "hitouts", "ff",
              "fa", "UF", "r50", "rPts")

intCols <- sapply(df, class) %>% .[grepl("integer", .)] %>% names()
df[intCols] <- sapply(df[intCols], as.numeric)

df[which(df$Season == "2020"),] <- df %>% filter(Season == 2020) %>% mutate(across(all_of(cols2020), function(x) x * 1.25))

# rolling ave columns

df <- df %>% group_by(name, providerId, Season)%>% mutate(L5 = rollapply(UF, width=5, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                         by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))
df <- df %>% group_by(name, providerId, Season)%>% mutate(L7 = rollapply(UF, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                         by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))
df <- df %>% group_by(name, providerId, Season)%>% mutate(L10 = rollapply(UF, width=10, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                          by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))
# other stat indicator column

df <- df %>% group_by(name, providerId, Season)%>% mutate(sMax = max(UF), sStDev = sd(UF), sDownDev = DownsideDeviation(UF, MAR = 90))

# jammy way to take the last observation as their season summary value

dfTemp <- df %>% group_by(name, providerId, Season) %>% mutate(L5 = tail(L5, 1), L7 = tail(L7, 1), L10 = tail(L10, 1), sStDev = tail(sStDev, 1), sDownDev = tail(sDownDev, 1))

#dfTemp <- dfTemp %>% ungroup()

dfTemp$sDownDev <- as.numeric(dfTemp$sDownDev)

# add in opponent difficulty

oppUF <- dfTemp %>% group_by(Season, Opp) %>% summarise(oppAvg = mean(UF))
oppUF$Season <- as.Date(oppUF$Season, format = "%Y") %m+% months(12)
oppUF$Season <- format(oppUF$Season, "%Y")

dfTemp <- left_join(dfTemp, oppUF, by = c("Season", "Opp"))

# Get 2023 fixture to identify team fixturing difficulty

fixture2023 <- fetch_fixture_afl(season = 2023)
fixture2023 <- fixture2023 %>% select("home.team.name", "away.team.name") %>% arrange(home.team.name) %>% rename("Team" = "home.team.name", "Opponent" = "away.team.name")
oppUF2023 <- oppUF %>% filter(Season == 2023)
fixture2023 <- left_join(fixture2023, oppUF2023, by = c("Opponent" = "Opp"))

# season summary data frame

seasonUF <- dfTemp %>% group_by(name, providerId, Season) %>% summarise(across(c("goals", "behinds", "kicks", "handballs", "disposals", 'marks', "tackles", "contPos", "uncontPos",
                                                                                 "i50", "hitouts", "ff", "fa", "UF", "r50", "rPts", "TOG", "KtH", "L5", "L7", "L10", "sStDev", 
                                                                                 "sDownDev", "oppAvg"), mean), across(c("posMatch", "posProvider", "team"), Mode), across(c("Age"), min), 
                                                                        across(c("sMax"), max))

rm("dfTemp")

# For the sake of better code structure and more logical modelling, we're going to create a 2023 data frame to append (rbind) to the bottom of the current
## data frame, this is because it's confusing how it's currently structured with 2022 as the forecasting year. A good exercise will be to code this
### out well and clear otherwise adding new variables is confusing like we experienced with the strength of schedule stuff

data2023 <- fetch_player_details(season = 2023)

data2023 <- data2023 %>% unite(name, firstName, surname, sep = " ")
data2023 <- data2023 %>% select(name, providerId, team, season, dateOfBirth)
data2023 <- data2023 %>% rename(Season = season)
data2023$Season <- as.character(data2023$Season)
data2023$Age <- as.numeric(round(difftime(as.Date("2023-03-14"), as.Date(data2023$dateOfBirth), units = "days") / 365.25, 1))
data2023 <- data2023 %>% select(-dateOfBirth)
seasonUF <- seasonUF %>% ungroup()
combSeason <- bind_rows(seasonUF, data2023)

### Lag all columns for the 2023 season

colstoLag <- colnames(combSeason[4:ncol(combSeason)])
colstoLag <- colstoLag[-grep(("Age"),colstoLag)]
colstoLag <- colstoLag[-grep(("team"),colstoLag)]
colstoLag <- colstoLag[-grep(("UF"),colstoLag)]
colstoLag <- colstoLag[-grep(("oppAvg"),colstoLag)]

### This was done to lag only specific columns, but maybe it's easier to just lag all columns and remove after

# testFrame <- mapply(lag_multiple, combSeason[,colstoLag], MoreArgs = list(1))
# combSeason <- cbind(combSeason, testFrame)
# combSeason[,-4:(-length(testFrame)-3)]

dfTest <- combSeason %>% ungroup() %>% group_by(providerId) %>% mutate_at(all_of(colstoLag), ~lag_multiple(., 1))
dfTest[,colstoLag] <- mapply(pull, dfTest[,colstoLag]) 
# colnames(dfTest1) <- colnames(dfTest)
dfTest1 <- dfTest %>% arrange(name, Season)
seasonUF <- dfTest1 %>% group_by(name, providerId) %>% mutate(lag_multiple(UF, 1), lag_multiple(UF, 2))
seasonUF <- seasonUF %>% ungroup() %>% group_by(Season, team, posProvider) %>% mutate(posComp = sum(lag1 >= 80))
colsSeasonUF <- c("name", "providerId", "season", "goalsL", "behindsL", "kicksL", "handballsL", "disposalsL", "marksL", "tacklesL",
                  "contPosL", "uncontPosL", "i50L", "hitoutsL", "ffL", "faL", "uf", "r50L", "rPtsL", "togL", "KtHL", "L5L", "L7L", "L10L",
                  "sStDevL", "sDownDevL", "oppAvg", "posMatchL", "posProviderL", "team", "age", "sMaxL", "ufL", "ufL2", "posComp")
colnames(seasonUF) <- colsSeasonUF
seasonUF <- seasonUF %>% select(name, providerId, season, team , age, uf, ufL, ufL2, goalsL, behindsL, kicksL, handballsL, disposalsL, marksL, tacklesL,
                                contPosL,  uncontPosL, i50L, hitoutsL, ffL, faL, r50L, rPtsL, togL, KtHL, L5L, L7L, L10L, sMaxL,
                                sStDevL, sDownDevL, oppAvg, posMatchL, posProviderL, posComp)
seasonUF <- seasonUF %>% ungroup() %>% group_by(providerId)
oppAvg2023 <- fixture2023 %>% group_by(Team) %>% summarise(Season = Season, oppAvg = mean(oppAvg)) %>% distinct()
result <- left_join(seasonUF, oppAvg2023, by = c("team" = "Team", "season" = "Season"))
result <- result %>% mutate_at(vars(oppAvg.x, oppAvg.y), funs(replace(., is.na(.), 0)))
result <- result %>% mutate(oppAvg = oppAvg.x + oppAvg.y)
seasonUF <- result %>% select(-c("oppAvg.y", "oppAvg.x", "posMatchL"))

factorCols <- c("season", "team", "posProviderL")
seasonUF[,factorCols] <- lapply(seasonUF[,factorCols], as.factor)


