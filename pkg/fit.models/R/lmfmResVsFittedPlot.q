lmfmResVsFittedPlot <- function(x, residuals.fun, smooths = FALSE,
                                rugplot = FALSE, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  fit <- lapply(x, fitted)
  res <- lapply(x, residuals.fun)
  n.res <- sapply(res, length)

  panel.special <- function(x, y, smooths, rugplot, id.n, ...)
  {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, smooths = smooths, rugplot = rugplot, id.n = id.n)
    panel.abline(h = 0, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(r = unlist(res), f = unlist(fit), mod = mod)

  p <- xyplot(r ~ f | mod,
              data = tdf,
              panel = panel.special,
              smooths = smooths,
              rugplot = rugplot,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


