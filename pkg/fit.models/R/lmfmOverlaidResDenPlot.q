lmfmOverlaidResDenPlot <- function(x, residuals.fun, lty, lwd, col, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(missing(lty))
    lty <- 1:n.models

  if(missing(lwd))
    lwd <- 1:n.models

  if(missing(col))
    col <- 1:n.models

  settings <- list(superpose.line = list(lty = lty, lwd = lwd, col = col))

  res <- lapply(x, residuals.fun)
  n.res <- sapply(res, length)
  mod <- factor(rep(mod.names, n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), mod = mod)

  p <- densityplot(~ res | "",
                   groups = mod,
                   data = tdf,
                   n = 256,
                   bw = "SJ",
                   plot.points = FALSE,
                   strip = function(...) strip.default(..., style = 1),
                   auto.key = list(corner = c(0.95, 0.95)),
                   par.settings = settings,
                   ...)

  print(p)
  invisible(p)
}


