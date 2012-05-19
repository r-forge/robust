print.lmRob <- function(x, digits = max(3, getOption("digits") - 3), ...) 
{
  if(x$est == "initial")
    cat("Initial Estimates.\n\n")

  cat("Call:\n")
  dput(x$call)

  coefs <- coef(x)

#  if(ns <- attr(coefs, "singular"))
#    cat("\nCoefficients: (", ns, 
#        " not defined because of singularities)\n", sep = "")
#
#  else 
    cat("\nCoefficients:\n")

  print(coefs, digits = digits, ...)

  rdf <- x$df.resid
  n <- length(x$residuals)

  cat("\nDegrees of freedom:", n, "total;", rdf, "residual\n")

  if(!is.null(x$na.action))
    cat(naprint(x$na.action),"\n")

  if(rdf > 0) {
    if(is.null(x$weights))
      cat("Residual standard error:", format(x$scale, digits = digits, ...),
          "\n")

    else 
      cat("Residual standard error (on weighted scale):", 
          format(x$scale, digits = digits, ...), "\n")
  }

  invisible(x)
}

