asmfmQQPlot <- function(x, robustQQline = TRUE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- sapply(x, function(u) !is.null(u$data))

  if(!any(data))
    stop("data is not available")

  data <- x[[(1:n.models)[data][1]]]$data
  n <- length(data)
  p <- (1:n) / (1 + n)
  quantiles <- matrix(0.0, n, n.models)

  for(j in 1:n.models) {
    quantiles[,j] <- switch(x[[j]]$distribution,
      gamma = {
        efmj <- coef(x[[j]])
        qgamma(p, shape = efmj[1], scale = efmj[2])
      },
      lognormal = {
        efmj <- coef(x[[j]])
        qlnorm(p, meanlog = efmj[1], sdlog = efmj[2])
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


