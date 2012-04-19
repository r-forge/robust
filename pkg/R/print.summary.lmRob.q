print.summary.lmRob <- function(x, ...) 
{
  if (x$est == "initial") 
    cat("Initial Estimates.\n")

  cat("\nCall: ")
  dput(x$call)
  resid <- x$residuals
  attr(resid, ".guiColInfo") <- NULL
  df <- x$df
  rdf <- df[2]

  if(rdf > 5) {
    cat("\nResiduals:\n")
    if(length(dim(resid)) == 2) {
      rq <- apply(t(resid), 1, quantile)
      dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", "Max"), 
                           dimnames(resid)[[2]])
    }

    else {
      rq <- quantile(resid)
      names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    print(rq, ...)
  }

  else if(rdf > 0) {
    cat("\nResiduals:\n")
    print(resid, ...)
  }

  if(nsingular <- df[3] - df[1])
    cat("\nCoefficients: (", nsingular, 
        " not defined because of singularities)\n", sep = "")

  else 
    cat("\nCoefficients:\n")

  if(!is.null(x$bootstrap)) {
    coef.names <- dimnames(x$coef)
    coef.names[[2]] <- c(coef.names[[2]][1:2],
      "Bootstrap SE", coef.names[[2]][3:4])
    the.coef <- cbind(x$coef[,1:2], x$bootstrap.se, x$coef[,3:4])
    dimnames(the.coef) <- coef.names
  }
  else
    the.coef <- x$coef

  print(format(the.coef, ...), quote = FALSE, ...)

  cat("\nResidual standard error:", format(signif(x$sigma, ...)), 
      "on",rdf, "degrees of freedom\n")

  if (!is.null(x$na.action))
    cat(naprint(x$na.action),"\n")

  if(!is.null(x$r.squared))
    cat("Multiple R-Squared:", 
        format(signif(x$r.squared, ...)),"\n")

  correl <- x$correlation

  if(!is.null(correl)) {
    p <- dim(correl)[2]
    if(p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      correl <- format(round(correl, ...), ...)
      correl[col(correl) > row(correl)] <- ""
      print(correl, quote = FALSE, ...)
    }
  }

  if(!is.null(x$biasTest)) {
    cat("\nTest for Bias:\n")
    print(x$biasTest)
  }

  invisible(x)
}


