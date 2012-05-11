plot.cov <- function(x, which.plots = "ask", ...)
{
  ######################################
  # move these args to ploting subfuns #
  ######################################
  #chisq.percent <- 0.975
  #id.n <- 3
  #variables
  ######################################

  ## cast x as a fit.models object with one element and plot ##
  ## it using plot.covfm ##

  x.name <- deparse(substitute(x))
  fm <- list(x = x)
  names(fm) <- x.name
  oldClass(fm) <- "covfm"

  plot(fm, which.plots = which.plots, ...)

  invisible(x)
}


