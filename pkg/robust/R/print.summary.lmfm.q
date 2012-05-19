print.summary.lmfm <- function(x, digits = max(3, getOption("digits") - 3),
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

  ## Not sure why I've got na.rm = TRUE here, can residuals be missing? ##
  resq <- t(sapply(x, function(u) quantile(resid(u), na.rm = TRUE)))
  dimnames(resq) <- list(fancy.names, c("Min", "1Q", "Median", "3Q", "Max"))

  cat("\nResidual Statistics:\n")
  print(resq, digits = digits, ...)

  coefs <- lapply(x, coef)
  cnames <- unique(unlist(lapply(coefs, rownames)))
  n.coefs <- length(cnames)
  coefmat <- matrix(NA, n.coefs * (n.models + 1) - 1, 4)
  r.names <- rep(c("", fancy.names), n.coefs)[-1]
  idx <- seq(from = 1, by = n.models + 1, length.out = n.coefs)
  r.names[idx] <- paste(paste(cnames, ":", sep = ""), r.names[idx])
  rownames(coefmat) <- format(r.names, justify = "right")
  colnames(coefmat) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  for(i in 1:n.models)
    coefmat[idx + (i-1), ][cnames %in% row.names(coefs[[i]]), ] <- coefs[[i]]

  cat("\nCoefficients:\n")
  printCoefmat(coefmat, digits = digits, signif.stars = signif.stars,
               na.print = "", ...)

  cat("\nResidual Scale Estimates:\n")
  for(i in 1:n.models)
    cat(fancy.names[i], format(x[[i]]$sigma, digits = digits, ...), "on",
        x[[i]]$df[2], "degrees of freedom\n")

  cat("\nMultiple R-squared:\n")
  for(i in 1:n.models)
    cat(fancy.names[i], format(x[[i]]$r.squared, digits = digits, ...), "\n")

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

  bias.test <- sapply(x, function(u) !is.null(u$biasTest))
  if(any(bias.test)) {
    cat("\nBias Tests for Robust Models:\n")
    bias.test <- (1:n.models)[bias.test]
    for(i in bias.test) {
      cat(mod.names[i], ":\n", sep = "")
      print(x[[i]]$biasTest, digits = digits, ...)
    }
  }

  invisible(x)
}


