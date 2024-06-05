library(lubridate)

isDate <- function(mydate, format=NULL) {
  if(is.null((format))) format =c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y") 
  tryCatch(!is.na(as.Date(mydate, "",tryFormats =format)),  
           error = function(err) {FALSE})  
}