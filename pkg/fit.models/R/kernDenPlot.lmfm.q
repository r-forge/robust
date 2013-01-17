kernDenPlot.lmfm <- function(x, fun, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  y <- lapply(x, fun)
  n.y <- sapply(y, length)

  panel.special <- function(x, y, ...)
  {
    panel.densityplot(x, ...)
    panel.abline(v = 0, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n.y), levels = mod.names)
  tdf <- data.frame(y = unlist(y), mod = mod)

  p <- densityplot(~ y | mod,
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


