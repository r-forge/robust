print.covRob <- function(x, ...)
{
  cat("Call:\n")
  dput(x$call)

  if (x$corr)
    cat("\nRobust Estimate of Correlation: \n")
  else
    cat("\nRobust Estimate of Covariance: \n")
  print(x$cov, ...)

  cat("\nRobust Estimate of Location: \n")
  print(x$center, ...) 

  invisible(x)
}


