plot.covRob <- function(x, which.plots = c(4, 3, 5), ...)
{
  x.name <- deparse(substitute(x))
  fm <- fit.models(x)
  names(fm) <- x.name

  plot(fm, which.plots = which.plots, ...)

  invisible(fm)
}


