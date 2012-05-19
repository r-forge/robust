coef.aovfm <- function(object, ...)
{
   oldClass(object) <- "aov"
   coef(object, ...)
}

