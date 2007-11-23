coef.glmRob <- function(object, ...)
{
   oldClass(object) <- "glm"
   coef(object, ...)
}


