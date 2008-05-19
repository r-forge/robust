print.covfm <- function(x, ...)
{
	n.models <- length(x)
	model.list <- attr(x, "model.list")
	mod.names <- format(names(model.list))

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		dput(x[[i]]$call)
	}

	p <- nrow(x[[1]]$cov)
	i1 <- rep(seq(p), times = p)
	i2 <- rep(seq(p), each = p)

	cov.index <- paste("[", paste(i1, i2, sep = ","), "]", sep = "")
	cov.index <- matrix(cov.index, p, p)
	cov.index <- cov.index[row(cov.index) >= col(cov.index)]

	cov.unique <- t(sapply(x, function(u) u$cov[row(u$cov) >= col(u$cov)]))
	dimnames(cov.unique) <- list(mod.names, cov.index)

	cat("\nComparison of Covariance/Correlation Estimates:\n")
	cat(" (unique covariance terms) \n")
	print(cov.unique, ...)

	center <- t(sapply(x, function(u) u$center))
	center.names <- names(x[[1]]$center)
	dimnames(center) <- list(mod.names, center.names)

	cat("\nComparison of Location Estimates: \n")
	print(center, ...)

	invisible(x)
}

