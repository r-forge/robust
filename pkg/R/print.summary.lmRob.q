print.summary.lmRob <- function(x, digits = max(3, getOption("digits") - 3),
                                signif.stars = getOption("show.signif.stars"),
                                ...) 
{
  if(x$est == "initial") 
    cat("Initial Estimates.\n")

  cat("\nCall:\n")
  cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  rdf <- x$df[2]

  if(rdf > 5) {
    cat("Residuals:\n")
    rq <- quantile(x$residuals)
    names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
    print(rq, digits = digits, ...)
  }

  else if(rdf > 0) {
    cat("\nResiduals:\n")
    print(x$residuals, digits = digits, ...)
  }

  if(nsingular <- x$df[3] - x$df[1])
    cat("\nCoefficients: (", nsingular, 
        " not defined because of singularities)\n", sep = "")

  else 
    cat("\nCoefficients:\n")

  coefs <- x$coefficients
  if(!is.null(aliased <- x$aliased) && any(aliased)) {
    cn <- names(aliased)
    coefs <- matrix(NA, length(aliased), 4)
    dimnames(coefs) <- list(cn, colnames(coefs))
    coefs[!aliased, ] <- x$coefficients
  }

  if(!is.null(x$bootstrap.se)) {
    dn <- dimnames(coefs)
    dn[[2]] <- c(dn[[2]][1:2], "Bootstrap SE", dn[[2]][3:4])
    coefs <- cbind(coefs[, 1:2, drop = FALSE], x$bootstrap.se,
                   coefs[, 3:4, drop = FALSE])
    dimnames(coefs) <- dn
  }

  printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
               na.print = "NA", ...)

  cat("\nResidual standard error:", format(signif(x$sigma, digits = digits, ...)), 
      "on", rdf, "degrees of freedom\n")

  if(!is.null(x$r.squared))
    cat("Multiple R-Squared:", format(x$r.squared, digits = digits, ...), "\n")

  correl <- x$correlation
  if(!is.null(correl)) {
    p <- NCOL(correl)
    if(p > 1) {
      cat("\nCorrelation of Coefficients:\n")
        correl <- format(round(correl, 2), nsmall = 2, digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
    }
  }
  cat("\n")

  if(!is.null(x$biasTest)) {
    cat("Test for Bias:\n")
    print(x$biasTest, digits = digits, ...)
  }

  if(!is.null(x$na.action))
    cat(naprint(x$na.action),"\n")

  invisible(x)
}


