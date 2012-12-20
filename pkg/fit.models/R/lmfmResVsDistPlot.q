lmfmResVsDistPlot <- function(x, residuals.fun, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, residuals.fun)
  n.res <- sapply(res, length)

  dists <- lapply(x, design.distance)

  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), dists = unlist(dists), mod = mod)

  panel.special <- function(x, y, id.n, ...) {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, id.n = id.n)
    invisible()
  }

  p <- xyplot(res ~ dists | mod,
              data = tdf,
              panel = panel.special,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


