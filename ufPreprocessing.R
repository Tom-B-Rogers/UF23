library(tidyverse)
library(fitzRoy)
library(zoo)
library(PerformanceAnalytics)

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

colsRemove2 <- c("firstName", "surname", "season", "jumperNumber", "heightInCm", "weightInKg", "homeTeam", "awayTeam")

df <- df %>% select(-colsRemove2)

df <- df %>% rename("posProvider" = "position.y", "posMatch" = "position.x", "team" = "team.x")

df$dateOfBirth <- as.Date(df$dateOfBirth)

df$Age = round(as.numeric((df$date - df$dateOfBirth) / 365.25), 1)

factorCols <- c("Season", "posProvider", "team")

df[, factorCols] <- lapply(df[, factorCols], factor)

df <- df %>% mutate(KtH = kicks / handballs)
df$KtH[is.infinite(df$KtH)] <- df$kicks[is.infinite(df$KtH)]
df <- df %>% filter(!is.na(KtH))

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

# season summary data frame

seasonUF <- dfTemp %>% group_by(name, providerId, Season) %>% summarise(across(c("goals", "behinds", "kicks", "handballs", "disposals", 'marks', "tackles", "contPos", "uncontPos","i50", "hitouts", "ff", "fa", "UF", "r50", "rPts", "TOG", "KtH", "L5", "L7", "L10", "sStDev", "sDownDev"), mean), 
                                                                        across(c("posMatch", "posProvider", "team"), Mode), 
                                                                        across(c("Age"), min), 
                                                                        across(c("sMax"), max))

rm("dfTemp")

seasonUF <- seasonUF %>% group_by(name, providerId) %>% mutate(lag_multiple(UF, 1), lag_multiple(UF, 2), lag_multiple(UF, 3))

seasonUF <- seasonUF %>% group_by(name, providerId) %>% mutate(lead_multiple(UF, 1))



















