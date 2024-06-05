is.package.loaded2 <- function (x, envir = sys.frame(sys.parent(0)), character.only = FALSE) 
{
  if (!character.only) {
    x <- atos(x, envir = envir)
    x <- strip(x, "'|\"")
  }
  sprintf("package:%s", x) %in% search()
}
