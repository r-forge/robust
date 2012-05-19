formula.lmRob <- function(x, ...)
{
  oldClass(x) <- "lm"
  formula(x, ...)
}


