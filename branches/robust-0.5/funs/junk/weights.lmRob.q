weights.lmRob <- function(object, ...)
{
  if(is.null(object$M.weights))
    warning("M.weights is only defined for S-estimates and MM-estimates.")

  object$M.weights
}


