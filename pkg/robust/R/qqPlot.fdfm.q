qqPlot.fdfm <- function(x, qqline = TRUE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- x[[1]]$x

  n <- length(data)
  p <- (1:n) / (1 + n)
  quantiles <- matrix(0.0, n, n.models)

  for(j in 1:n.models)
    quantiles[,j] <- do.call(paste("q", x[[j]]$densfun, sep = ""),
                             c(list(p = p), x[[j]]$estimate))

  mod <- factor(rep(mod.names, each = n), levels = mod.names)
  tdf <- data.frame(quantiles = as.vector(quantiles),
                    data = rep(sort(data), n.models),
                    mod = mod)

  panel.special <- function(x, y, qqline = TRUE, ...)
  {
    panel.xyplot(x, y, ...)

    if(qqline) {
      u <- quantile(x[!is.na(x)], c(0.25, 0.75))
      v <- quantile(y[!is.na(y)], c(0.25, 0.75))
      slope <- diff(v) / diff(u)
      int <- v[1] - slope * u[1]
      panel.abline(int, slope)
    }

    invisible()
  }

  p <- xyplot(data ~ quantiles | mod,
              data = tdf,
              panel = panel.special,
              qqline = qqline,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


