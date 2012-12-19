lmfmResKernDenPlot <- function(x, residuals.fun, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, residuals.fun)
  n.res <- sapply(res, length)

  panel.special <- function(x, y, ...)
  {
    panel.densityplot(x, ...)
    panel.abline(v = 0, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), mod = mod)

  p <- densityplot(~ res | mod,
                   data = tdf,
                   n = 256,
                   bw = "SJ",
                   plot.points = FALSE,
                   panel = panel.special,
                   strip = function(...) strip.default(..., style = 1),
                   layout = c(n.models, 1, 1),
                   ...)

  print(p)
  invisible(p)
}


