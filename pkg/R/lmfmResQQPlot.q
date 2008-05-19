lmfmResQQPlot <- function(x, type = "response", envelope = TRUE,
  half.normal = FALSE, n.samples = 100, level = .95, id.n = 3,
  robustQQline = TRUE, main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Normal QQ Plot of Residuals"

  if(missing(xlab))
    xlab <- "Quantiles of Standard Normal"

  if(missing(ylab))
    ylab <- "Residuals"

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))

  std.resids <- sapply(x, residuals, type = type)
  sigmas <- sapply(x, function(u) ifelse(is.null(u$sigma), NA, u$sigma))

  if(!any(is.na(sigmas)))
    std.resids <- sweep(std.resids, 2, sigmas, "/")

	px <- py <- matrix(0.0, n, n.models)

	for(i in 1:n.models) {
		tmp <- qqnorm(std.resids[, i], plot.it = FALSE)
		px[, i] <- tmp$x
		py[, i] <- tmp$y
	}

## calculate alpha / 2 ##

  level <- (1 - level) / 2

## which order statistics to use for envelope ##

  sample.ci <- function(x, n.samples, level) {
    lower <- round(level * n.samples) + 1
    upper <- round((1 - level) * n.samples)
    sort(x)[c(lower, upper)]
  }

  if(envelope && half.normal) {
    py <- abs(py)
      px <- .5 + (0:(n-1)) / (2*n)
    px <- matrix(rep(qnorm(px), n.models), n, n.models)
    for(i in 1:n.models)
      px[order(py[,i]),i] <- px[,i]
    envelope <- matrix(abs(rnorm(n.samples * n)), n, n.samples)
    envelope <- apply(envelope, 2, function(x) sort(x))
    envelope <- t(apply(envelope, 1, sample.ci,
                  n.samples = n.samples, level = level))
    den.range <- c(min(py, envelope), max(py, envelope))
  }

  else if(envelope) {
    envelope <- matrix(rnorm(n.samples * n), n, n.samples)
    envelope <- apply(envelope, 2, function(x) sort(x))
    envelope <- t(apply(envelope, 1, sample.ci,
                  n.samples = n.samples, level = level))
    den.range <- c(min(py, envelope), max(py, envelope))
  }

  else
    den.range <- c(min(py), max(py))

  if(length(envelope) > 1 && half.normal) {
    ordered.px <- matrix(0, n, n.models)

    for(i in 1:n.models)
      ordered.px[order(py[,i]),i] <- px[,i]

    df <- data.frame(
      py = c(as.vector(py),
              rep(envelope[,1], n.models),
              rep(envelope[,2], n.models)),
      px = c(as.vector(px), rep(as.vector(ordered.px), 2)),
      grp = c(rep("data", n * n.models),
              rep("min", n * n.models),
              rep("max", n * n.models)),
      mod = rep(rep(mod.names, rep(n, n.models)), 3))

      panel.special <- function(x, y, id.n, robQQln, ...) {
        dat.idx <- 1:(length(x)/3)

        panel.xyplot(x[dat.idx], y[dat.idx], pch = 16, col = 6, ...)

        if(robQQln)
          panel.abline(coef(lmRob(y[dat.idx] ~ x[dat.idx])))

        panel.addons(x[dat.idx], y[dat.idx], smooths = FALSE, rugplot = FALSE,
          id.n = id.n)

        dat.idx <- ((length(x)/3)+1):(2*length(x)/3)

        panel.xyplot(sort(x[dat.idx]), sort(y[dat.idx]), type = "l", col = 1,
          lty = 2, ...)

        dat.idx <- (2*(length(x)/3)+1):(length(x))

        panel.xyplot(sort(x[dat.idx]), sort(y[dat.idx]), type = "l", col = 1,
          lty = 2, ...)

        invisible()
      }
    }

    else if(length(envelope) > 1) {
      df <- data.frame(
        py = c(	as.vector(py), rep(envelope[,1], n.models),
          rep(envelope[,2], n.models)),
        px = rep(as.vector(px), 3),
        grp = c(rep("data", n * n.models), rep("min", n * n.models),
          rep("max", n * n.models)),
        mod = rep(rep(mod.names, rep(n, n.models)), 3))

      panel.special <- function(x, y, id.n, robQQln, ...) {
        dat.idx <- 1:(length(x)/3)

        panel.xyplot(x[dat.idx], y[dat.idx], pch = 16, col = 6, ...)

        panel.addons(x[dat.idx], y[dat.idx], smooths = FALSE, rugplot = FALSE,
          id.n = id.n)

        if(robQQln)
          panel.abline(coef(lmRob(y[dat.idx] ~ x[dat.idx])))

        dat.idx <- ((length(x)/3)+1):(2*length(x)/3)

        panel.xyplot(sort(x[dat.idx]), sort(y[dat.idx]), type = "l", col = 1,
          lty = 2, ...)

        dat.idx <- (2*(length(x)/3)+1):(length(x))

        panel.xyplot(sort(x[dat.idx]), sort(y[dat.idx]), type = "l", col = 1,
          lty = 2, ...)

        invisible()
      }
    }

    else {
      df <- data.frame(py = as.vector(py), px = as.vector(px),
        mod = rep(mod.names, each = n))

      panel.special <- function(x, y, id.n, robQQln) {
        panel.xyplot(x, y, pch = 16, col = 6)
        panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)

        if(robQQln)
          panel.abline(coef(lmRob(y ~ x)))

        invisible()
      }
    }

  print(xyplot(py ~ px | mod,
            data = df,
            xlab = xlab,
            ylab = ylab,
            main = main,
            id.n = id.n,
            robQQln = robustQQline,
            panel = panel.special,
            strip = function(...)
              strip.default(..., style = 1),
            layout = c(n.models, 1, 1),
            ...))

  invisible(x)
}


