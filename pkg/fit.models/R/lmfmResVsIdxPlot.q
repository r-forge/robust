lmfmResVsIdxPlot <- function(x, type = "response", level = 0.95, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, resid, type = type)
  n.res <- sapply(res, length)

  s <- numeric(n.models)
  for(i in 1:n.models) {
    if(is.null(x[[i]]$scale))
      s[i] <- sqrt(sum(res[[i]]^2) / x[[i]]$df.residual)
    else
      s[i] <- x[[i]]$scale

    res[[i]] <- c(s[i], res[[i]])
  }

  indices <- lapply(x, function(u) attributes(model.frame(u))$row.names)

  for(i in 1:n.models) {
    if(class(indices[[i]]) != "integer")
      indices[[i]] <- 0:n.res[i]
    else
      indices[[i]] <- c(0, indices[[i]])
  }

  y.range <- max(c(abs(range(unlist(res)))), s * qnorm(0.5 + level / 2.0))
  y.range <- 1.05 * c(-y.range, y.range)

  panel.special <- function(x, y, level = level, id.n = 3, ...)
  {
    s <- y[1]
    x <- x[-1]
    y <- y[-1]
    type <- ifelse(length(y) > 60, "l", "b")
    panel.xyplot(x, y, type = type, ...)
    s <- s * qnorm(0.5 + level / 2.0)
    if(id.n > 0) {
      n <- length(y)
      out <- order(abs(y))[(n - id.n + 1):n]
      out <- out[abs(y)[out] >= s]
      panel.text(x[out], y[out], paste(" ", x[out], sep = ""), adj = 0)
    }
    panel.abline(h = s, lty = 2)
    panel.abline(h = -s, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, 1 + n.res), levels = mod.names)
  tdf <- data.frame(t = unlist(indices), res = unlist(res), mod = mod)

  p <- xyplot(res ~ t | mod,
              data = tdf,
              ylim = y.range,
              panel = panel.special,
              level = level,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


