print.glmfm <- function(x, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  cat("\nCalls: \n")
  for(i in 1:n.models) {
    cat(mod.names[i], ": ")
    print(x[[i]]$call)
  }

  mod.coefs <- lapply(x, coef)
  coef.names <- unique(unlist(lapply(mod.coefs, names)))
  n.coefs <- length(coef.names)
  tmp <- matrix(NA, n.coefs, n.models)
  dimnames(tmp) <- list(coef.names, mod.names)

  for(i in 1:n.models)
    tmp[match(names(mod.coefs[[i]]), coef.names), i] <- mod.coefs[[i]]

  cat("\nCoefficients:\n")
  print(tmp, ...)
  cat("\n")

  cat("Residual Deviance Estimates:\n")
  for(i in 1:n.models)
    cat(mod.names[i], ":", format(signif(x[[i]]$deviance)),
      "on", x[[i]]$df.resid, "degrees of freedom\n")

  invisible(x)
}


