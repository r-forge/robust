print.summary.asmDstn <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	cat(x$header)
	cat("\nCall: ")
	print(x$call)
	cat("\nParameter Estimates:\n")
	print(x$coefficients, digits = digits)

  if(!is.null(x$mu) && !is.null(x$V.mu)) {
    tab <- c(x$mu, x$V.mu)
    names(tab) <- c("mean", "var(mean)")
    cat("\nEstimated Mean:\n")
    print(tab, digits = digits)
  }

  if(!is.null(x$vcov)) {
    cat("\nParameter covariance matrix estimate:\n")
    print(x$vcov, digits = digits)
  }

	invisible(x)
}


