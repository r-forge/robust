deviance.lmRob <- function(object, ...) {
  z <- object$dev
  if(is.null(z))
    warning("Deviance is only defined for S-estimates and MM-estimates.")
  z
}


