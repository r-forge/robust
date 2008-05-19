model.frame.lmRob <- function(formula, ...)
{
  oldClass(formula) <- "lm"
  model.frame(formula, ...)
}


