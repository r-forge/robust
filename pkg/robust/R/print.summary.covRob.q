print.summary.covRob <- function(x, digits = max(3, getOption("digits") - 3),
                                 print.distance = TRUE, ...)
{
  cat("Call:\n")
  dput(x$call)

  if(x$corr)
    cat("\nRobust Estimate of Correlation: \n")
  else
    cat("\nRobust Estimate of Covariance: \n")
  print(x$cov, digits = digits, ...)

  cat("\nRobust Estimate of Location: \n")
  print(x$center, digits = digits, ...)

  cat("\nEigenvalues: \n")
  print(x$evals, digits = digits, ...)

  if(print.distance && !is.null(x$dist)) {
    cat("\nSquared Robust Distances: \n")
    print(x$dist, digits = digits, ...)
  }

  invisible(x)
}


