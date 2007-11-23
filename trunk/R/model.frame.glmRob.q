model.frame.glmRob <- function(formula, ...)
{
  oldClass(formula) <- "glm"
  model.frame(formula, ...)
}


