covfmSqrtMDPlot <- function(x, chisq.percent = 0.975, id.n = 3, main, xlab,
                            ylab,  ...)
{
	n.models <- length(x)
	mod.names <- names(x)

  dist.extractor <- function(u) u$dist
  dists <- sqrt(as.vector(sapply(x, dist.extractor)))

  n <- length(x[[1]]$dist)
  p <- dim(x[[1]]$cov)[1]

  vt <- sqrt(qchisq(chisq.percent, df = p))
  y.range <- c(0, max(dists))
  y.range[2] <- max(y.range[2], 1.1 * vt)

  if(missing(main))
    main <- ""

  if(missing(xlab))
    xlab <- "Index"

  if(missing(ylab))
    ylab <- "Square Root of Mahalanobis Distance"

  panel.special <- function(x, y, vt = 2.5, id.n = 3) {
    panel.xyplot(x, y, pch = 16)
    n <- length(y)
    yp.idx <- which(y > vt)
    id.n <- min(id.n, length(yp.idx))
    if(id.n > 0) {
      yp.idx <- order(y)[(n-id.n+1):n]
      if(any(yp.idx))
        panel.text(x[yp.idx], y[yp.idx], paste(" ", yp.idx, sep = ""), adj = 0)
    }
    panel.abline(h = vt, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  tdf <- data.frame(dists = dists,
                    lab = rep(1:n, n.models),
                    mod = mod)

  p <- xyplot(dists ~ lab | mod,
              data = tdf,
              main = main,
              xlab = xlab,
              ylab = ylab,
              panel = panel.special,
              strip = function(...) strip.default(..., style = 1),
              id.n = id.n,
              vt = vt,
              ...)

  print(p)
  invisible(p)
}

