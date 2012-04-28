print.summary.asmDstn <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	cat(x$header)
	cat("\nCall: ")
	print(x$call)
	cat("\nParameter Estimates:\n")
	print(x$coefficients, digits = digits)
  cat("\nvar(mu): ")
  cat(format(x$V.mu, digits = digits))
  cat("\n")

  if(!is.null(x$cov)) {
    cat("\nCovariance matrix estimate for alpha and sigma:\n")
    print(x$cov, digits = digits)
  }

	invisible(x)
}
