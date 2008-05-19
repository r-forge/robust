print.summary.covfm <- function(x, print.distance = FALSE, ...)
{
	n.models <- length(x)
	mod.names <- format(names(x))
	model.list <- attr(x, "model.list")

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	p <- dim(x[[1]]$cov)[1]
	i1 <- rep(seq(p), times = p)
	i2 <- rep(seq(p), each = p)

	cov.index <- paste("[", paste(i1, i2, sep = ","), "]", sep = "")
	cov.index <- matrix(cov.index, p, p)
	cov.index <- cov.index[row(cov.index) >= col(cov.index)]

	cov.unique <- t(sapply(x, function(u) u$cov[row(u$cov) >= col(u$cov)]))
	dimnames(cov.unique) <- list(mod.names, cov.index)

	cat("\nComparison of Covariance/Correlation Estimates:\n")
	cat(" (unique correlation terms) \n")
	print(cov.unique, ...)

	center <- t(sapply(x, function(u) u$center))
	center.names <- names(x[[1]]$center)
	dimnames(center) <- list(mod.names, center.names)

	cat("\nComparison of Location Estimates: \n")
	print(center, ...)

	evals <- t(sapply(x, function(u) u$evals))
	eval.names <- names(x[[1]]$evals)
	dimnames(evals) <- list(mod.names, eval.names)

	cat("\nComparison of Eigenvalues: \n")
	print(evals, ...)

	have.dist <- sapply(x, function(u) !is.null(u$dist))
	if(print.distance && all(have.dist)) {
		dists <- t(sapply(x, function(u) u$dist))
		dimnames(dists) <- list(mod.names, names(x[[1]]$dist))
		cat("\nComparison of Mahalanobis Distances: \n")
		print(dists, ...)
	}

	invisible(x)
}

