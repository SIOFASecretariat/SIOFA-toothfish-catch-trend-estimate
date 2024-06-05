cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}
