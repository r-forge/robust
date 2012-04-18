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
  model.list <- list(x$call)
  names(model.list) <- x.name
  fm <- list(x = x)
  names(fm) <- x.name
  attr(fm, "model.list") <- model.list
  oldClass(fm) <- "covfm"

  plot(fm, which.plots = which.plots, ...)

  invisible(x)
}


