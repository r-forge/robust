plot.fitdstn <- function(x, which.plots = 2:3, ...)
{
  x.name <- deparse(substitute(x))
  fm <- fit.models(x)
  names(fm) <- x.name

  plot(fm, which.plots = which.plots, ...)

  invisible(fm)
}

