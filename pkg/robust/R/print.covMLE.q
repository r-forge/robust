print.covClassic <- function(x, digits = max(3, getOption("digits") - 3), ...)
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

  invisible(x)
}


