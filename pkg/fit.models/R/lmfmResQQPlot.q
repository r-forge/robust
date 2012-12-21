lmfmResQQPlot <- function(x, residuals.fun, envelope = TRUE,
                          half.normal = FALSE, n.samples = 250, level = .95,
                          id.n = 3, qqline = TRUE, ...)
{
  confidence.envelope <- function(n, sd = 1, n.samples = 250, level = 0.95,
                                  half.normal = FALSE)
  {
    lower <- upper <- matrix(0.0, n, n.models)

    alphaOver2 <- (1.0 - level) / 2.0
    probs <- c(alphaOver2, 1.0 - alphaOver2)

    env <- matrix(rnorm(n * n.samples, sd = sd), n.samples, n)

    if(half.normal)
      env <- abs(env)

    env <- apply(env, 1, sort)
    env <- apply(env, 1, quantile, probs = probs)

    list(lower = env[1, ], upper = env[2, ])
  }

  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, residuals.fun)
  n.res <- sapply(res, length)

  px <- py <- list()
  for(i in 1:n.models) {
    tmp <- qqnorm(res[[i]], plot.it = FALSE)
    px[[i]] <- tmp$x
    py[[i]] <- tmp$y
  }

  if(half.normal) {
    py <- lapply(py, abs)
    px <- lapply(n.res, function (u) .5 + (0:(u-1)) / (2*u))
    px <- lapply(px, qnorm)
    for(i in 1:n.models)
      px[[i]][order(py[[i]])] <- px[[i]]
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

    env <- list()
    for(i in 1:n.models)
      env[[i]] <- confidence.envelope(n.res[i], sigma.hats[i], n.samples,
                                      level, half.normal)
    lower <- lapply(env, function(u) u$lower)
    upper <- lapply(env, function(u) u$upper)

    den.range <- c(min(unlist(py), unlist(lower)),
                   max(unlist(py), unlist(upper)))
  }

  else
    den.range <- c(min(unlist(py)), max(unlist(py)))

  if(envelope && half.normal) {
    mod <- factor(rep(rep(mod.names, n.res), 3), levels = mod.names)

    tdf <- data.frame(py = c(unlist(py),
                             unlist(lower),
                             unlist(upper)),
                      px = rep(unlist(px), 3),
                      mod = mod)

    panel.special <- function(x, y, id.n, qqline, ...) {
      dat.idx <- 1:(length(x)/3)

      panel.xyplot(x[dat.idx], y[dat.idx], ...)

      if(qqline) {
        u <- quantile(x[!is.na(x)], c(0.25, 0.75))
        v <- quantile(y[!is.na(y)], c(0.25, 0.75))
        slope <- diff(v) / diff(u)
        int <- v[1] - slope * u[1]
        panel.abline(int, slope)
      }

      panel.addons(x[dat.idx], y[dat.idx], id.n = id.n)

      dat.idx <- ((length(x)/3)+1):(2*length(x)/3)

      llines(sort(x[dat.idx]), sort(y[dat.idx]), col.line = "black", lty = 2)

      dat.idx <- (2*(length(x)/3)+1):(length(x))

      llines(sort(x[dat.idx]), sort(y[dat.idx]), col.line = "black", lty = 2)

      invisible()
    }
  }

  else if(envelope) {
    mod <- factor(rep(rep(mod.names, n.res), 3), levels = mod.names)

    tdf <- data.frame(py = c(unlist(py),
                             unlist(lower),
                             unlist(upper)),
                      px = rep(unlist(px), 3),
                      mod = mod)

    panel.special <- function(x, y, id.n, qqline, ...) {
      dat.idx <- 1:(length(x)/3)

      panel.xyplot(x[dat.idx], y[dat.idx], ...)

      panel.addons(x[dat.idx], y[dat.idx], id.n = id.n)

      if(qqline) {
        u <- quantile(x[!is.na(x)], c(0.25, 0.75))
        v <- quantile(y[!is.na(y)], c(0.25, 0.75))
        slope <- diff(v) / diff(u)
        int <- v[1] - slope * u[1]
        panel.abline(int, slope)
      }

      #if(robQQln)
      #  panel.abline(coef(lmRob(y[dat.idx] ~ x[dat.idx])))

      dat.idx <- ((length(x)/3)+1):(2*length(x)/3)

      llines(sort(x[dat.idx]), sort(y[dat.idx]), col.line = "black", lty = 2)

      dat.idx <- (2*(length(x)/3)+1):(length(x))

      llines(sort(x[dat.idx]), sort(y[dat.idx]), col.line = "black", lty = 2)

      invisible()
    }
  }

  else {
    mod <- factor(rep(mod.names, n.res), levels = mod.names)
    tdf <- data.frame(px = unlist(px), py = unlist(py), mod = mod)

    panel.special <- function(x, y, id.n, qqline, ...) {
      panel.xyplot(x, y, ...)
      panel.addons(x, y, id.n = id.n)

      if(qqline) {
        u <- quantile(x[!is.na(x)], c(0.25, 0.75))
        v <- quantile(y[!is.na(y)], c(0.25, 0.75))
        slope <- diff(v) / diff(u)
        int <- v[1] - slope * u[1]
        panel.abline(int, slope)
      }

      #if(robQQln)
      #  panel.abline(coef(lmRob(y ~ x)))

      invisible()
    }
  }

  p <- xyplot(py ~ px | mod,
              data = tdf,
              id.n = id.n,
              qqline = qqline,
              panel = panel.special,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


