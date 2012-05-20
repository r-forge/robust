print.summary.glmfm <- function(x, digits = max(3, getOption("digits") - 3),
                                signif.stars = getOption("show.signif.stars"),
                                ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  fancy.names <- format(paste(mod.names, ":", sep = ""), justify = "right")

  cat("\nCalls:\n")
  for(i in 1:n.models) {
    cat(fancy.names[i], " ", sep = "")
    print(x[[i]]$call, ...)
  }

  resq <- t(sapply(x, function(u) quantile(u$deviance.resid, na.rm = TRUE)))
  dimnames(resq) <- list(fancy.names, c("Min", "1Q", "Median", "3Q", "Max"))

  cat("\nDeviance Residuals:\n")
  print(resq, digits = digits, ...)

  coefs <- lapply(x, coef)
  cnames <- unique(unlist(lapply(coefs, rownames)))
  n.coefs <- length(cnames)
  coefmat <- matrix(NA, n.coefs * (n.models + 1) - 1, 4)
  r.names <- rep(c("", fancy.names), n.coefs)[-1]
  idx <- seq(from = 1, by = n.models + 1, length.out = n.coefs)
  r.names[idx] <- paste(paste(cnames, ":", sep = ""), r.names[idx])
  rownames(coefmat) <- format(r.names, justify = "right")
  colnames(coefmat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
browser()
  for(i in 1:n.models)
    coefmat[idx + (i-1), ][cnames %in% row.names(coefs[[i]]), ] <- coefs[[i]]

  cat("\nCoefficients:\n")
  printCoefmat(coefmat, digits = digits, signif.stars = signif.stars,
               na.print = "", ...)

  devs <- format(sapply(x, function(u) u$null.deviance), digits = digits, ...)
  dfs <- format(sapply(x, function(u) u$df.null), digits = digits, ...)

  cat("\nNull deviance:\n")
  for(i in 1:n.models)
    cat(" ", fancy.names[i], devs[i], "on", dfs[i], "degrees of freedom\n")

  devs <- format(sapply(x, function(u) u$deviance), digits = digits, ...)
  dfs <- format(sapply(x, function(u) u$df[2]), digits = digits, ...)

  cat("\nResidual deviance:\n")
  for(i in 1:n.models)
    cat(" ", fancy.names[i], devs[i], "on", dfs[i], "degrees of freedom\n")

  correlations <- lapply(x, function(u) u$correlation)
  if(all(!sapply(correlations, is.null))) {    
    if(any(sapply(correlations, NCOL) > 1)) {
      cat("\nCorrelations:\n")
      for(i in 1:n.models) {
        if((p <- NCOL(correlations[[i]])) > 1) {        
          correl <- format(round(correlations[[i]], 2), nsmall = 2,
                           digits = digits, ...)
          correl[!lower.tri(correl)] <- ""
          cat(mod.names[i], ":\n", sep = "")
          print(correl[-1, -p, drop = FALSE], quote = FALSE, ...)
        }
        cat("\n")
      }
    }
  }

  invisible(x)
}


