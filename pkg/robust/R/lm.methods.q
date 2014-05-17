residuals.lmRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  residuals(object, ...)
}


deviance.lm <- function (object, ...)
{
  oldClass(object) <- "lm"
  deviance(object, ...)
}


model.matrix.lmRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  model.matrix(object, ...)
}


model.frame.lmRob <- function(formula, ...)
{
  oldClass(formula) <- "lm"
  model.frame(formula, ...)
}


print.lmRob <- function(x, ...) 
{
  oldClass(x) <- "lm"
  print(x, ...)
}


