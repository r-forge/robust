residuals.lmRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  residuals(object, ...)
}


