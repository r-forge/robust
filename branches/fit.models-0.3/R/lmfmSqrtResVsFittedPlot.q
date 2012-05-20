lmfmSqrtResVsFittedPlot <- function(x, type = "response", smooths = FALSE,
                                    rugplot = FALSE, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  panel.special <- function(x, y, smooths, rugplot, id.n = 3, ...)
  {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, smooths, rugplot, id.n = id.n)
    invisible()
  }

  fit <- lapply(x, fitted)
  res <- lapply(x, function(u) sqrt(abs(resid(u))))
  n.res <- sapply(res, length)

  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), fit = unlist(fit), mod = mod)

  p <- xyplot(res ~ fit | mod,
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


