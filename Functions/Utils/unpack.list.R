unpack.list <- function(object) {
  for(.x in names(object)){
    assign(value = object[[.x]], x=.x, envir = parent.frame())
  }
} 