model.matrix.lmRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  model.matrix(object, ...)
}


