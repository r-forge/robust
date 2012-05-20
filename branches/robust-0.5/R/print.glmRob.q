print.glmRob <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  if(!is.null(x$call)) {
    cat("Call:\n")
    dput(x$call)
  }

  coef <- x$coef
  if(any(nas <- is.na(coef))) {
    if(is.null(names(coef)))
      names(coef) <- paste("b", 1:length(coef), sep = "")        
    cat("\nCoefficients: (", sum(nas), " not defined because of singularities)\n",
      sep = "")
  }

  else
    cat("\nCoefficients:\n")

  print(coef, digits = digits, ...)

  rank <- x$rank
  if(is.null(rank))
    rank <- sum(!nas)

  nobs <- length(x$residuals)
  rdf <- x$df.resid

  if(is.null(rdf))
    rdf <- nobs - rank

  cat("\nDegrees of Freedom:", nobs, "Total;", rdf, "Residual\n")
  cat("Residual Deviance:", format(x$deviance, digits = digits), "\n")

  invisible(x)
}



