lmfmResQQPlot <- function(x, type = "response", envelope = TRUE,
                          half.normal = FALSE, n.samples = 250, level = .95,
                          id.n = 3, robustQQline = TRUE, main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Normal QQ Plot of Residuals"

  if(missing(xlab))
    xlab <- "Standard Normal Quantiles"

  if(missing(ylab))
    ylab <- "Ordered Residuals"

  normal.simulation.envelope <- function(n, sd = 1, n.samples = 250,
                                         level = 0.95, half.normal = FALSE)
  {
    n.models <- length(sd)
    lower <- upper <- matrix(0.0, n, n.models)

    alphaOver2 <- (1.0 - level) / 2.0
    probs <- c(alphaOver2, 1.0 - alphaOver2)

    for(j in 1:n.models) {
      env <- matrix(rnorm(n * n.samples, sd = sd[j]), n.samples, n)

      if(half.normal)
        env <- abs(env)

      env <- apply(env, 1, sort)
      env <- apply(env, 1, quantile, probs = probs)

      lower[, j] <- env[1, ]
      upper[, j] <- env[2, ]
    }

    list(lower = lower, upper = upper)
  }

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))

  resids <- sapply(x, resid, type = type)

	px <- py <- matrix(0.0, n, n.models)

	for(i in 1:n.models) {
		tmp <- qqnorm(resids[, i], plot.it = FALSE)
		px[, i] <- tmp$x
		py[, i] <- tmp$y
	}

  if(half.normal) {
    py <- abs(py)
    px <- .5 + (0:(n-1)) / (2*n)
    px <- matrix(rep(qnorm(px), n.models), n, n.models)
    for(i in 1:n.models)
      px[order(py[, i]), i] <- px[, i]
  }

  if(envelope) {
    sigma.hats <- numeric(n.models)
    for(i in 1:n.models) {
      if(!is.null(x[[i]]$scale))
        sigma.hats[i] <- x[[i]]$scale
      else {
        x.sum <- summary(x[[i]])
        if(!is.null(x.sum$dispersion))
          sigma.hats[i] <- sqrt(x.sum$dispersion)
        else if(!is.null(x.sum$sigma))
          sigma.hats[i] <- x.sum$sigma
        else
          stop("unable to determine residual scale")
      }
    }

    env <- normal.simulation.envelope(n, n.samples = n.samples, sd = sigma.hats,
                                      half.normal = half.normal, level = level)
    den.range <- c(min(py, env$lower), max(py, env$upper))
  }

  else
    den.range <- c(min(py), max(py))

  if(envelope && half.normal) {
    ordered.px <- matrix(0, n, n.models)

    for(i in 1:n.models)
      ordered.px[order(py[,i]),i] <- px[,i]

    mod <- factor(rep(rep(mod.names, rep(n, n.models)), 3), levels = mod.names)

    df <- data.frame(
      py = c(as.vector(py), as.vector(env$lower), as.vector(env$upper)),
      px = c(as.vector(px), rep(as.vector(ordered.px), 2)),
      grp = c(rep("data", n * n.models),
              rep("min", n * n.models),
              rep("max", n * n.models)),
      mod = mod)

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

  else if(envelope) {

    mod <- factor(rep(rep(mod.names, rep(n, n.models)), 3), levels = mod.names)

    df <- data.frame(
      py = c(as.vector(py), as.vector(env$lower), as.vector(env$upper)),
      px = rep(as.vector(px), 3),
      grp = c(rep("data", n * n.models),
              rep("min", n * n.models),
              rep("max", n * n.models)),
      mod = mod)

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

    mod <- factor(rep(mod.names, each = n), levels = mod.names)

    df <- data.frame(py = as.vector(py),
                     px = as.vector(px),
                     mod = mod)

    panel.special <- function(x, y, id.n, robQQln, ...) {
      panel.xyplot(x, y, pch = 16, col = 6, ...)
      panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)

      if(robQQln)
        panel.abline(coef(lmRob(y ~ x)))

      invisible()
    }
  }

  p <- xyplot(py ~ px | mod,
              data = df,
              xlab = xlab,
              ylab = ylab,
              main = main,
              id.n = id.n,
              robQQln = robustQQline,
              panel = panel.special,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


