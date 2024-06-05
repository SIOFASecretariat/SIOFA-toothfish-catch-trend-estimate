
### Load RData 
loadRData <- function(fileName) {
  #loads an RData file, and returns it
  load(fileName)
  #print(ls())
  #n <- readline(prompt="Which variable to load? \n")
  get(ls()[-which(grepl('fileName', ls()))])
}