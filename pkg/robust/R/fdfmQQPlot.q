fdfmQQPlot <- function(x, robustQQline = TRUE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- attributes(x)$x

  n <- length(data)
  p <- (1:n) / (1 + n)
  quantiles <- matrix(0.0, n, n.models)

  for(j in 1:n.models) {
    quantiles[,j] <- switch(attributes(x)$distribution,
      gamma = {
        do.call("qgamma", c(list(p = p), as.list(x[[j]]$estimate)))
      },
      lognormal = {
        do.call("qlnorm", c(list(p = p), as.list(x[[j]]$estimate)))
      },
      weibull = {}
    )
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)
  tdf <- data.frame(quantiles = as.vector(quantiles),
                    data = rep(sort(data), n.models),
                    mod = mod)

  panel.special <- function(x, y, robustQQline = TRUE, ...)
  {
    panel.xyplot(x, y, ...)
    if(robustQQline)
      panel.abline(coef(lmRob(y ~ x, trace = FALSE)))
    invisible()
  }

  p <- xyplot(data ~ quantiles | mod,
              data = tdf,
              panel = panel.special,
              robustQQline = robustQQline,
              strip = function(...) strip.default(..., style = 1),
              ...)

  print(p)
  invisible(p)
}


