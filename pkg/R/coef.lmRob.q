coef.lmRob <- function(object, ...)
{
   oldClass(object) <- "lm"
   coef(object, ...)
}


