asmfmQQPlot <- function(x, robustQQline = TRUE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  y <- sapply(x, function(u) !is.null(u$data))

  if(!any(y))
    stop("data is not available")

  y <- x[[(1:n.models)[y][1]]]$data
  n <- length(y)
  p <- (1:length(y)) / (1 + length(y))
  q <- matrix(0.0, n, n.models)

  for(j in 1:n.models) {
    q[,j] <- switch(x[[j]]$distribution,
      gamma = qgamma(p, shape = x[[j]]$alpha, scale = x[[j]]$sigma),
      lognormal = {},
      weibull = {}
    )
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)
  tdf <- data.frame(q = as.vector(q),
                    y = rep(sort(y), n.models),
                    mod = mod)

  panel.special <- function(x, y, robustQQline = TRUE, ...)
  {
    panel.xyplot(x, y, ...)
    if(robustQQline)
      panel.abline(coef(lmRob(y ~ x, trace = FALSE)))
    invisible()
  }

  p <- xyplot(y ~ q | mod,
              data = tdf,
              panel = panel.special,
              robustQQline = robustQQline,
              strip = function(...) strip.default(..., style = 1),
              ...)

  print(p)
  invisible(p)
}


