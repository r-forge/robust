plot.glmRob <- function(x, which.plots = "ask", ...)
{
  x.name <- deparse(substitute(x))
  model.list <- list(x$call)
  names(model.list) <- x.name
  x <- list(x = x)
  names(x) <- x.name
  attr(x, "model.list") <- model.list
  oldClass(x) <- "glmfm"

  plot.glmfm(x, which.plots = which.plots, ...)

  invisible(x[[1]])
}



