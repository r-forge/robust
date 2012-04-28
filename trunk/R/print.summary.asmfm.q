print.summary.asmfm <- function(x, digits = max(3, getOption("digits") - 3),
                                 ...)
{
	n.models <- length(x)
  mod.names <- format(names(x))

	cat("Calls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ", sep = "")
		print(x[[i]]$call)
	}

	coefs <- lapply(x, coef)
	coef.names <- names(coefs[[1]])
	n.coefs <- length(coef.names)
  coefs <- matrix(unlist(coefs), n.models, n.coefs, byrow = TRUE)
	dimnames(coefs) <- list(paste(mod.names, ":", sep = ""), coef.names)

	cat("\nCoefficients:\n")
  print(coefs, digits = digits, ...)

  V.mus <- matrix(sapply(x, function(u) u$V.mu), ncol = 1)
  dimnames(V.mus) <- list(paste(mod.names, ":", sep = ""), "var(mu)")

	cat("\nVariance Estimates:\n")
  print(V.mus, digits = digits, ...)
	cat("\n")

	invisible(x)
}

