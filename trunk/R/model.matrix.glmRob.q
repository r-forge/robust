model.matrix.glmRob <- function(object, ...)
{
  oldClass(object) <- "glm"
  model.matrix(object, ...)
}



