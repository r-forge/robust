lmfmResKernDenPlot <- function(x, n = 512, main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Kernel Density Estimate of Residuals"

  if(missing(xlab))
    xlab <- "Residuals"

  if(missing(ylab))
    ylab <- "Density"

  n.models <- length(x)
  mod.names <- names(x)

  res <- sapply(x, residuals)
  denx <- deny <- matrix(0, n, n.models)

  for(i in 1:n.models) {
    den <- density(res[, i], bw = "SJ", n = n, na.rm = TRUE)
    denx[, i] <- den$x
    deny[, i] <- den$y
  }

  panel.special <- function(x, y)
  {
    panel.xyplot(x, y, type = "l", col = 6)
    panel.abline(v = 0, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(denx = as.vector(denx),
                   deny = as.vector(deny),
                   mod = mod)

  p <- xyplot(deny ~ denx | mod,
              data = df,
              xlab = xlab,
              ylab = ylab,
              panel = panel.special,
              main = main,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


