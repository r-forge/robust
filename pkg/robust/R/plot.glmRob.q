plot.glmRob <- function(x, which.plots = c(2, 5, 7, 6), ...)
{
  fm <- fit.models(x)
  names(fm) <- deparse(substitute(x))

  plot.glmfm(fm, which.plots = which.plots, ...)

  invisible(x)
}



