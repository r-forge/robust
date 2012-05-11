plot.glmRob <- function(x, which.plots = "ask", ...)
{
  x.name <- deparse(substitute(x))
  fm <- list(x = x)
  names(x) <- x.name
  oldClass(x) <- "glmfm"

  plot.glmfm(x, which.plots = which.plots, ...)

  invisible(x[[1]])
}



