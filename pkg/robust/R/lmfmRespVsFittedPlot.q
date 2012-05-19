lmfmRespVsFittedPlot <- function(x, smooths = FALSE, rugplot = FALSE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  fit <- lapply(x, fitted)
  resp <- lapply(x, function(u) model.extract(model.frame(u), "response"))
  n.resp <- sapply(resp, length)

  panel.special <- function(x, y, smooths, rugplot, ...)
  {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, smooths = smooths, rugplot = rugplot, id.n = 0)
    panel.abline(0, 1, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n.resp), levels = mod.names)
  tdf <- data.frame(y = unlist(resp), f = unlist(fit), mod = mod)

  p <- xyplot(y ~ f | mod,
              data = tdf,
              panel = panel.special,
              smooths = smooths,
              rugplot = rugplot,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


