print.summary.covRob <- function(x, print.distance = TRUE, ...)
{
  cat("Call:\n")
  dput(x$call)

  if(x$corr)
    cat("\nRobust Estimate of Correlation: \n")
  else
    cat("\nRobust Estimate of Covariance: \n")
  print(x$cov, ...)

  cat("\nRobust Estimate of Location: \n")
  print(x$center, ...)

  cat("\nEigenvalues: \n")
  print(x$evals, ...)

  if(print.distance && !is.null(x$dist)) {
    cat("\nRobust Distances: \n")
    print(x$dist, ...)
  }

  invisible(x)
}


