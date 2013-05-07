print.summary.covClassic <- function(x,
                                     digits = max(3, getOption("digits") - 3),
                                     print.distance = TRUE, ...)
{
  cat("Call:\n")
  dput(x$call)

  if(x$corr)
    cat("\nClassical Estimate of Correlation: \n")
  else 
    cat("\nClassical Estimate of Covariance: \n")
  print(x$cov, digits = digits, ...)

  cat("\nClassical Estimate of Location: \n")
  print(x$center, digits = digits, ...)

  cat("\nEigenvalues: \n")
  print(x$evals, digits = digits, ...)

  if(print.distance && !is.null(x$dist)) {
    cat("\nSquared Mahalanobis Distances: \n")
    print(x$dist, digits = digits, ...)
  }

  invisible(x)
}

