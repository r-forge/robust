summary.asmDstn <- function(object, ...)
{
	orig <- class(object)[1]
  object$coefficients <- coef(object)
  object <- object[c("call", "header", "coefficients", "mu", "V.mu", "vcov")] 
	oldClass(object) <- c(paste("summary", orig, sep = "."), "summary.asmDstn")
	object
}

