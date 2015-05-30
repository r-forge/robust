scatterPlot.lmfm <- function(object, x.fun, y.fun, ...)
{
  n.models <- length(object)
  mod.names <- names(object)

  x <- lapply(object, x.fun)
  n.x <- sapply(x, length)
  y <- lapply(object, y.fun)

  mod <- factor(rep(mod.names, n.x), levels = mod.names)
  tdf <- data.frame(x = unlist(x), y = unlist(y), mod = mod)

  panel.special <- function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.addons(x, y)
    invisible()
  }

  p <- xyplot(y ~ x | mod,
              data = tdf,
              panel = panel.special,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


