lmfmResVsIdxPlot <- function(x, type = "response", level = 0.95, id.n = 3,
                             main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Residuals vs. Index (Time)"

  if(missing(xlab))
    xlab <- "Index (Time)"

  if(missing(ylab))
    ylab <- "Residuals"

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))
  threshold <- qnorm(level)

  resids <- sapply(x, resid, type = type)

  y.range <- range(resids)
  y.range[1] <- 1.05 * min(y.range[1], -threshold)
  y.range[2] <- 1.05 * max(y.range[2], threshold)

  panel.special <- function(x, y, threshold = 1.645, id.n = 3)
  {
    n <- length(y)
    type <- ifelse(n > 60, "l", "b")
    panel.xyplot(x, y, type = type, col = 6, pch = 16)
    outliers <- which(abs(y) > threshold)
    if(length(outliers) > id.n)
      outliers <- order(abs(y))[(n - id.n + 1):n]
    if(id.n > 0 && any(outliers))
      panel.text(x[outliers], y[outliers], paste(" ", outliers, sep = ""), adj = 0)
    panel.abline(h = threshold, lty = 2)
    panel.abline(h = -threshold, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(indicies = rep(1:n, n.models),
                   resids = as.vector(resids),
                   mod = mod)

  p <- xyplot(resids ~ indicies | mod,
              data = df,
              xlab = xlab,
              panel = panel.special,
              id.n = id.n,
              ylim = y.range,
              ylab = ylab,
              main = main,
              strip = function(...) strip.default(..., style = 1),
              threshold = threshold,
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


