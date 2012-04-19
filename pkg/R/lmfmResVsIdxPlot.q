lmfmResVsIdxPlot <- function(x, type = "response", level = 0.95, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  threshold <- qnorm(level)

  res <- as.matrix(sapply(x, resid, type = type))
  n <- nrow(res)

  y.range <- range(res)
  y.range[1] <- 1.05 * min(y.range[1], -threshold)
  y.range[2] <- 1.05 * max(y.range[2], threshold)

  panel.special <- function(x, y, threshold = 1.645, id.n = 3, ...)
  {
    n <- length(y)
    type <- ifelse(n > 60, "l", "b")
    panel.xyplot(x, y, type = type, ...)
    out <- which(abs(y) > threshold)
    if(length(out) > id.n)
      out <- order(abs(y))[(n - id.n + 1):n]
    if(id.n > 0 && any(out))
      panel.text(x[out], y[out], paste(" ", out, sep = ""), adj = 0)
    panel.abline(h = threshold, lty = 2)
    panel.abline(h = -threshold, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(indicies = rep(1:n, n.models),
                   res = as.vector(res),
                   mod = mod)

  p <- xyplot(res ~ indicies | mod,
              data = df,
              panel = panel.special,
              id.n = id.n,
              ylim = y.range,
              strip = function(...) strip.default(..., style = 1),
              threshold = threshold,
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


