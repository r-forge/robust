print.summary.glmfm <- function(x, ...)
{
	n.models <- length(x)
  mod.names <- names(x)
	fancy.names <- format(mod.names, justify = "right")
	model.list <- attr(x, "model.list")

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(fancy.names[i], ": ", sep = "")
		print(x[[i]]$call, ...)
	}

	resid.qrtls <- t(sapply(x, function(u)
    quantile(u$deviance.resid, na.rm = TRUE)))

	dimnames(resid.qrtls) <- list(paste(fancy.names, ":", sep = ""),
    c("Min", "1Q", "Median", "3Q", "Max"))

	cat("\nDeviance Residuals:\n")
	print(resid.qrtls, ...)

  coefs <- lapply(x, function(u) u$coefficients)
  coef.names <- format(dimnames(coefs[[1]])[[1]], justify = "right")
  p <- length(coef.names)
  coef.matrix <- matrix(as.numeric(NA), n.models * p, 4)
  dimnames(coef.matrix) <- list(paste(fancy.names, rep(coef.names, each = 2)),
    c("Value", "Std. Error", "t value", "Pr(>|t|)"))

  for(i in 1:n.models) {
    row.indices <- seq(from = i, by = n.models, length = p)
    coef.matrix[row.indices, 1:(dim(coefs[[i]])[2])] <- coefs[[i]]
  }

	cat("\nCoefficients:\n")
  print(coef.matrix, ...)

  cat("\nResidual Deviance:\n")
  for(i in 1:n.models)
    cat(fancy.names[i], ": ", format(x[[i]]$deviance, ...), " on ", x[[i]]$df[2],
      " degrees of freedom\n", sep = "")

  if(all(sapply(x, function(u) !is.null(u$correlation)))) {
    correlations <- lapply(x, function(u) u$correlation)
    if(dim(correlations[[1]])[1] > 1) {
      cat("\nCorrelation of Coefficients:\n")
      for(i in 1:n.models) {
        cormat <- format(correlations[[i]][2:p, 1:(p-1)], ...)
        cormat[row(cormat) < col(cormat)] <- ""
        cat(mod.names[i], ":\n", sep = "")
        print(cormat, quote = FALSE)
        cat("\n")
      }
    }
  }

  invisible(x)
}


