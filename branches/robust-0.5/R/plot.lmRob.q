plot.lmRob <- function(x, which.plots = c(5, 2, 6, 4), ...)
{
  fm <- fit.models(x)
  names(fm) <- deparse(substitute(x))

  plot(fm, which.plots = which.plots, ...)

  invisible(x)
}


