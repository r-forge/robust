print.summary.glmRob <- function(x, digits = max(3, getOption("digits") - 3),
                                 ...)
{
  nas <- x$nas
  coef <- x$coef
  correl <- x$correl

  if(any(nas)) {
    nc <- length(nas)
    cnames <- names(nas)
    coef1 <- array(NA, c(nc, 3), list(cnames, dimnames(coef)[[2]]))
    coef1[!nas, ] <- coef
    coef <- coef1

    if(!is.null(correl)) {
      correl1 <- matrix(NA, nc, nc, dimnames = list(cnames,cnames))
      correl1[!nas, !nas] <- correl
      correl <- correl1
    }
  }

  cat("\nCall: ")
  dput(x$call)

  dresid <- x$deviance.resid
  n <- length(dresid)
  rdf <- x$df[2]

  if(rdf > 5) {
    cat("Deviance Residuals:\n")
    rq <- quantile(as.vector(dresid), na.rm = TRUE)
    names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
    print(rq, digits = digits, ...)
  }

  else if(rdf > 0) {
    cat("Deviance Residuals:\n")
    print(dresid, digits = digits, ...)
  }

  if(any(nas))
    cat("\nCoefficients: (", sum(nas),
      " not defined because of singularities)\n", sep = "")
  else
    cat("\nCoefficients:\n")

  print(coef, digits = digits, ...)

  cat(paste("\n(Dispersion Parameter for", names(x$dispersion),
    "family taken to be", format(round(x$dispersion, ...)), ")\n"))

  int <- attr(x$terms, "intercept")

  if(is.null(int))
    int <- 1

  cat("\n    Null Deviance:", format(x$null.deviance, digits = digits, ...),
    "on", n - int, "degrees of freedom\n")

  cat("\nResidual Deviance:", format(x$deviance, ...), "on",
    format(rdf, digits = digits, ...), "degrees of freedom\n")

  cat("\nNumber of Iterations:", format(trunc(x$iter)), "\n")

  if(!is.null(correl)) {
    p <- dim(correl)[2]

    if(p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      ll <- lower.tri(correl)
      correl[ll] <- format(correl[ll], digits = digits, ...)
      correl[!ll] <- ""
      print(correl[-1, -p, drop = FALSE], quote = FALSE, ...)
    }
  }

  invisible(x)
}


