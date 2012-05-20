print.covRob <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("Call:\n")
  dput(x$call)

  if (x$corr)
    cat("\nRobust Estimate of Correlation: \n")
  else
    cat("\nRobust Estimate of Covariance: \n")
  print(x$cov, digits = digits, ...)

  cat("\nRobust Estimate of Location: \n")
  print(x$center, digits = digits, ...) 

  invisible(x)
}


