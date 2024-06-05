mid.matrix <- function(m) {
  nr <- nrow(m)
  nc <- ncol(m)
  if (nr %% 2 == 0) {
    ns <- c(nr %/% 2, nr %/% 2 + 1) 
  } else {
    ns <- nr %/% 2 + 1
  }
  
  if (nc %% 2 == 0) {
    ms <- c(nc %/% 2, nc %/% 2 + 1)
  } else {
    ms <- nc %/% 2 + 1
  }
  
  return(list(position.matrix = c(ns, ms) , position.vect= (ns*nc)- (nc-ms), value = m[ns, ms]) )
}