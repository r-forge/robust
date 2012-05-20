plot.covMLE <- function(x, which.plots = "ask", ...)
{
  x.name <- deparse(substitute(x))
  fm <- fit.models(x)
  names(fm) <- x.name

  plot(fm, which.plots = which.plots, ...)

  invisible(fm)
}


