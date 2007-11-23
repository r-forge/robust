print.summary.cov <- function(x, print.distance = TRUE, ...)
{
  cat("Call:\n")
  dput(x$call)

  if(x$corr)
    cat("\nClassical Estimate of Correlation: \n")
  else 
    cat("\nClassical Estimate of Covariance: \n")
  print(x$cov, ...)

  cat("\nClassical Estimate of Location: \n")
  print(x$center,...)

  cat("\nEigenvalues: \n")
  print(x$evals, ...)

  if(print.distance && !is.null(x$dist)) {
    cat("\nClassical Mahalanobis Distances: \n")
    print(x$dist, ...)
  }

  invisible(x)
}

