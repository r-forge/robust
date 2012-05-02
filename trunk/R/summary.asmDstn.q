summary.asmDstn <- function(object, ...)
{
	orig <- class(object)[1]
  object <- object[c("call", "header", "estimate", "mu", "V.mu", "vcov")] 
	oldClass(object) <- c(paste("summary", orig, sep = "."), "summary.asmDstn")
	object
}

