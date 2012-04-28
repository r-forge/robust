summary.asmDstn <- function(object, ...)
{
	orig <- class(object)[1]
  object$coefficients <- coef(object)
  object <- object[c("call", "header", "coefficients", "V.mu", "cov")] 
	oldClass(object) <- c(paste("summary", orig, sep = "."), "summary.asmDstn")
	object
}

