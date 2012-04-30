coef.asmDstn <- function(object, ...)
{
  coef.names <- object$parameter.names
  coefs <- unlist(object[coef.names])
	names(coefs) <- coef.names
  coefs
}


