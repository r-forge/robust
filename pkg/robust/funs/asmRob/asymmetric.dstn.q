
print.asymmetric.dstn <- function(x, ...)
{
	tmp <- matrix(c(x$alpha, x$sigma, x$mu), 3, 1)
	dimnames(tmp) <- list(c('Alpha', 'Sigma', 'Mu'), c('Estimates'))
	cat("\n")
	cat(x$header)
	cat("\nCall: ")
	dput(x$call)
	print(tmp)
	invisible(tmp)
}


plot.asymmetric.dstn <- function(x, truncate = T, which.plots = "ask",
	lower.q = 0.999, plot.data = T, robustQQline = T, ...)
{
	x.name <- deparse(substitute(x))
	model.list <- list(x$call)
	names(model.list) <- x.name
	x <- list(x = x)
	names(x) <- x.name
	attr(x, "model.list") <- model.list

	plot.asymfm(x, truncate = truncate, which.plots = which.plots,
		lower.q = lower.q, plot.data = plot.data, robustQQline =
		robustQQline, ...)

	invisible(x[[1]])
}


summary.asymmetric.dstn <- function(object, ...)
{
	orig.class <- class(object)
	ans <- list()
	ans$call <- object$call
	ans$header <- object$header
	ans$coefficients <- matrix(NA, 3, 2)
	ans$coefficients[,1] <- c(object$alpha, object$sigma, object$mu)

	if(!is.null(object$cov))
		ans$coefficients[,2] <- c(sqrt(diag(object$cov)), sqrt(object$V.mu))
	else
		ans$coefficients[,2] <- c(NA, NA, sqrt(object$V.mu))

	dimnames(ans$coefficients) <- list(c("Alpha", "Sigma", "Mu"), 
			c("Estimates", "Std. Error"))

	oldClass(ans) <- paste("summary", orig.class, sep = ".")
	ans
}


print.summary.asymmetric.dstn <- function(x, ...)
{
	cat("\n")
	cat(x$header)
	cat("\n")
	cat("\nCall:\n  ")
	print(x$call)
	cat("\nCoefficients:\n")
	cf <- x$coefficients
	cf <- format(cf)
	cf[strip.blanks(cf) == "NA"] <- ""
	prmatrix(cf, quote = F)
	invisible(x)
}


coef.asymmetric.dstn <- function(object, ...)
{
	tmp <- c(object$alpha, object$sigma, object$mu)
	names(tmp) <- c("Alpha", "Sigma", "Mu")
	tmp
}
