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

  dev.res <- lapply(x, function(u) u$deviance.resid)
  has.dev.res <- which(!sapply(dev.res, is.null))

  if(length(has.dev.res)) {
    dev.res <- t(sapply(dev.res[has.dev.res], quantile))
    dimnames(dev.res) <- list(fancy.names[has.dev.res],
                              c("Min", "1Q", "Median", "3Q", "Max"))
    cat("\nDeviance Residuals:\n")
    print(dev.res, digits = digits, ...)
  }

  coefs <- lapply(x, coef)
  cnames <- unique(unlist(lapply(coefs, rownames)))
  n.coefs <- length(cnames)
  coefmat <- matrix(NA, n.coefs * (n.models + 1) - 1, 4)
  r.names <- rep(c("", fancy.names), n.coefs)[-1]
  idx <- seq(from = 1, by = n.models + 1, length.out = n.coefs)
  r.names[idx] <- paste(paste(cnames, ":", sep = ""), r.names[idx])
  rownames(coefmat) <- format(r.names, justify = "right")
  colnames(coefmat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  for(i in 1:n.models)
    coefmat[idx + (i-1), ][cnames %in% row.names(coefs[[i]]), ] <- coefs[[i]]

  cat("\nCoefficients:\n")
  printCoefmat(coefmat, digits = digits, signif.stars = signif.stars,
               na.print = "", ...)

  null.devs <- sapply(x, function(u) u$null.deviance)
  has.null.dev <- which(!sapply(null.devs, is.null))
  null.df <- sapply(x, function(u) u$df.null)
  null.devs <- format(null.devs, digits = digits, ...)
  null.df <- format(null.df, digits = digits, ...)

  if(length(has.null.dev)) {
    cat("\nNull deviance:\n")
    for(i in has.null.dev)
      cat(" ", fancy.names[i], null.devs[i], "on", null.df[i],
          "degrees of freedom\n")
  }

  devs <- sapply(x, function(u) u$deviance)
  has.dev <- which(!sapply(devs, is.null))
  df <- sapply(x, function(u) u$df[2])
  devs <- format(devs, digits = digits, ...)
  df <- format(df, digits = digits, ...)

  if(length(has.dev)) {
    cat("\nResidual deviance:\n")
    for(i in has.null.dev)
      cat(" ", fancy.names[i], devs[i], "on", df[i], "degrees of freedom\n")
  }

  cat("\n")

  correlations <- lapply(x, function(u) u$correlation)
  if(all(!sapply(correlations, is.null))) {    
    if(any(sapply(correlations, NCOL) > 1)) {
      cat("Correlations:\n")
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


