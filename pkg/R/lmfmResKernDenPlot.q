lmfmResKernDenPlot <- function(x, ...)
{
  mod.names <- names(x)

  res <- as.matrix(sapply(x, residuals))
  mod <- factor(rep(mod.names, each = nrow(res)), levels = mod.names)

  panel.special <- function(x, y, ...)
  {
    panel.densityplot(x, ...)
    panel.abline(v = 0, lty = 2)
    invisible()
  }

  tdf <- data.frame(res = as.vector(res),
                    mod = mod)

  p <- densityplot(~ res | mod,
                   data = tdf,
                   n = 256,
                   bw = "SJ",
                   plot.points = FALSE,
                   panel = panel.special,
                   strip = function(...) strip.default(..., style = 1),
                   ...)

  print(p)
  invisible(p)
}


