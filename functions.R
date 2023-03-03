Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

lag_multiple <- function(x, n_vec){
  map(n_vec, lag, x = x) %>% 
    set_names(paste0("lag", n_vec)) %>% 
    as_tibble()
}

lead_multiple <- function(x, n_vec){
  map(n_vec, lead, x = x) %>% 
    set_names(paste0("lead", n_vec)) %>% 
    as_tibble()
}


funcUnique = function(x){
  as.character(x)[!(duplicated(as.character(x))|duplicated(as.character(x), fromLast=TRUE))]
}