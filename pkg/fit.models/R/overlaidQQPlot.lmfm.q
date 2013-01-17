overlaidQQPlot.lmfm <- function(x, fun, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  settings <- list(superpose.symbol = list(pch = 1:n.models, col = 1:n.models))

  y <- lapply(x, fun)
  n.y <- sapply(y, length)
  mod <- factor(rep(mod.names, n.y), levels = mod.names)
  tdf <- data.frame(y = unlist(y), mod = mod)

  p <- qqmath(~ y | "",
              groups = mod,
              data = tdf,
              distribution = qnorm,
              strip = function(...) strip.default(..., style = 1),
              auto.key = list(corner = c(0.05, 0.95)),
              par.settings = settings,
              ...)

  print(p)
  invisible(p)
}


