dummy.coef.aovRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  dummy.coef(object)
}

