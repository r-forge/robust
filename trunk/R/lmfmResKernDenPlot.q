lmfmResKernDenPlot <- function(x, ...)
{
  bandwidth.nrd <- function(x) {
    r <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    h <- (r[2] - r[1])/1.34
    4 * 1.06 * min(sqrt(var(x, na.rm = TRUE)), h) * length(x)^{-1/5}
  }

  n.models <- length(x)
  mod.names <- names(x)

	res <- sapply(x, residuals)
	denx <- deny <- matrix(0, 100, n.models)

	for(i in 1:n.models) {
		b <- bandwidth.nrd(res[, i])
		den <- density(res[, i], width = b, n = 100, na.rm = TRUE)
		denx[, i] <- den$x
		deny[, i] <- den$y
	}

  panel.special <- function(x, y)
  {
    panel.xyplot(x, y, type = "l", col = 6)
    panel.abline(v = 0, lty = 2)
    invisible()
  }
    
  df <- data.frame(denx = as.vector(denx),
      deny = as.vector(deny),
      mod = rep(mod.names, rep(100, n.models)))
        
  print(xyplot(deny ~ denx | mod,
    data = df,
    xlab = "Residuals",
    ylab = "Kernel Density",
    panel = panel.special,
    main = "Kernel Density of Residuals",
    strip = function(...) strip.default(..., style = 1),
    layout = c(n.models, 1, 1),
    ...))

  invisible(x)
}


