lmfmOverlaidResDenPlot <- function(x, main, xlab, ylab, ...)
{
  bandwidth.nrd <- function(x) {
    r <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    h <- (r[2] - r[1])/1.34
    4 * 1.06 * min(sqrt(var(x, na.rm = TRUE)), h) * length(x)^{-1/5}
  }

  n.models <- length(x)
  mod.names <- names(x)

  if(missing(main))
    main <- "Kernel Density of Residuals"

  if(missing(xlab))
    xlab <- "Residuals"

  if(missing(ylab))
    ylab <- "Kernel Density"

	res <- sapply(x, residuals)
	denx <- deny <- matrix(0, 100, n.models)

	for(i in 1:n.models) {
		b <- bandwidth.nrd(res[, i])
		den <- density(res[, i], width = b, n = 100, na.rm = TRUE)
		denx[, i] <- den$x
		deny[, i] <- den$y
	}

  matplot(denx, deny,
    type = "l",
    xlab = xlab,
    ylab = ylab,
    main = main,
    lty = 1:n.models,
    col = 1:n.models,
    lwd = n.models:1,
    ...)

  key(max(denx), max(deny),
    text = list(mod.names),
    lines = list(type = "l",
              col = 1:n.models,
              lty = 1:n.models,
              lwd = n.models:1),
    corner = c(1, 1),
    transparent = TRUE)

  invisible(x)
}


