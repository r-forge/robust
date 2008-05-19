print.cov <- function(x, ...)
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

  invisible(x)
}


