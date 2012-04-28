print.asmDstn <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	coefs <- coef(x)
	cat(x$header)
	cat("\nCall: ")
	dput(x$call)
	print(coefs, digits = digits)
	invisible(x)
}


