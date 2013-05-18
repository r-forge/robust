residuals.glmRob <- function(object, ...)
{
  oldClass(object) <- "glm"
  residuals(object)
}


model.matrix.glmRob <- function(object, ...)
{
  oldClass(object) <- "lm"
  model.matrix(object)
}


model.frame.glmRob <- function(formula, ...)
{
  oldClass(formula) <- "glm"
  model.frame(formula)
}


print.glmRob <- function(x, ...) 
{
  oldClass(x) <- "glm"
  print(x)
}


family.glmRob <- function(object, ...)
{
  oldClass(object) <- "glm"
  family(object)
}


hatvalues.glmRob <- function(model, ...)
  robMD2(model, scale = "md2", X = model$x)


