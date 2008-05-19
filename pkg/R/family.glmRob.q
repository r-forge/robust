family.glmRob <- function(object, ...)
{
  oldClass(object) <- "glm"
  family(object, ...)
}


