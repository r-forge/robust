coef.asmDstn <- function(object, ...)
{
  coefs <- c(object$alpha, object$sigma, object$mu)
	names(coefs) <- c("alpha", "sigma", "mu")
  coefs
}


