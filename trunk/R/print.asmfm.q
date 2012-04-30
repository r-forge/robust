print.asmfm <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
	n.models <- length(x)
	mod.names <- format(names(x))

	cat("Calls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	coefs <- lapply(x, coef)
	coef.names <- names(coefs[[1]])
	n.coefs <- length(coef.names)
  coefs <- matrix(unlist(coefs), n.models, n.coefs, byrow = TRUE)
	dimnames(coefs) <- list(paste(mod.names, ":"), coef.names)

	cat("\nCoefficients:\n")
	print(coefs, digits = digits, ...)
	cat("\n")

	invisible(x)
}


