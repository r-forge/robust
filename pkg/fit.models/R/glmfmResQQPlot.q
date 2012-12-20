glmfmResQQPlot <- function(x, type = "deviance", id.n = 3, qqline = TRUE, ...)
{
  n.models <- length(x)
  families <- sapply(x, function(u) family(u)$family)

  res <- lapply(x, residuals, type = type)
  fit <- lapply(x, fitted)
  n.fit <- sapply(fit, length)
  mod.names <- names(x)

  qq.a <- y <- list()

  for(i in 1:n.models) {
    y[[i]] <- x[[i]]$y
    qq.a[[i]] <- numeric(n.fit[[i]])
    qq.a[[i]][order(res[[i]])] <- qqplot.glmRob(y[[i]], fit[[i]], families[i])$quantiles
  }

  panel.special <- function(x, y, qqline = TRUE, id.n = 3) {
    panel.xyplot(x, y, pch = 16)
    panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)

    if(qqline) {
      u <- quantile(x[!is.na(x)], c(0.25, 0.75))
      v <- quantile(y[!is.na(y)], c(0.25, 0.75))
      slope <- diff(v) / diff(u)
      int <- v[1] - slope * u[1]
      panel.abline(int, slope)
    }

    invisible()
  }

  mod <- factor(rep(mod.names, n.fit), levels = mod.names)

  tdf <- data.frame(qq.a = unlist(qq.a), qq.b = unlist(res), mod = mod)

  p <- xyplot(qq.a ~ qq.b | mod,
              data = tdf,
              panel = panel.special,
              qqline = qqline,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


