
SIOFA_Season <- function(date, start_month=12, start_day=1) {
    year <- year(date)
    month <- month(date)
    day <- day(date)
  
  Season = ifelse(month <start_month | (month=start_month &  day <start_day), year, year +1) 
  
  Season
}


