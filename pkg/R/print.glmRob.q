print.glmRob <- function(x, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
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

  print(coef, ...)

  rank <- x$rank
  if(is.null(rank))
    rank <- sum(!nas)

  nobs <- length(x$residuals)
  rdf <- x$df.resid

  if(is.null(rdf))
    rdf <- nobs - rank

  cat("\nDegrees of Freedom:", nobs, "Total;", rdf, "Residual\n")
  cat("Residual Deviance:", format(x$deviance), "\n")

  invisible(x)
}



