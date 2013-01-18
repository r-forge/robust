indexPlot.lmfm <- function(x, fun, level = 0.95, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  y <- lapply(x, fun)
  n.y <- sapply(y, length)

  s <- numeric(n.models)
  for(i in 1:n.models) {
    if(is.null(x[[i]]$scale))
      s[i] <- sqrt(sum(y[[i]]^2) / x[[i]]$df.residual)
    else
      s[i] <- x[[i]]$scale
  }

  indices <- lapply(x, function(u) attributes(model.frame(u))$row.names)

  if(all(sapply(indices, class) == "integer")) {
    newx <- min(unlist(indices)):max(unlist(indices))
    for(i in 1:n.models) {
      newy <- rep(as.numeric(NA), length(newx))
      newy[indices[[i]]] <- y[[i]]
      indices[[i]] <- newx
      y[[i]] <- newy
    }
  }

  else
    indices <- lapply(indices, function(u) 1:length(u))

  n.y <- sapply(y, length)

  for(i in 1:n.models) {
    y[[i]] <- c(s[i], y[[i]])
    indices[[i]] <- c(NA, indices[[i]])
  }

  y.range <- max(c(abs(range(unlist(y)))), s * qnorm(0.5 + level / 2.0))
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
      panel.text(x[out], y[out], paste(" ", x[out], sep = ""), adj = 0, ...)
    }
    panel.abline(h = s, lty = 2)
    panel.abline(h = -s, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, 1 + n.y), levels = mod.names)
  tdf <- data.frame(t = unlist(indices), y = unlist(y), mod = mod)

  p <- xyplot(y ~ t | mod,
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


