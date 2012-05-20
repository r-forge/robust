lmfmResVsRDPlot <- function(x, type = "response", level = 0.95, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  res <- lapply(x, resid, type = type)
  n.res <- sapply(res, length)

  compute.distances <- function(obj, fun = covRob)
  {
    mf <- model.frame(obj)
    mm <- model.matrix(obj)

    if(is.null(mf) || is.null(mm))
      return(NA)

    numeric.vars <- sapply(mf, is.numeric)
    numeric.vars <- names(numeric.vars)[numeric.vars]
    keep <- intersect(numeric.vars, colnames(mm))
    p <- length(keep)

    if(p < 1) {
      warning("could not compute robust distances")
      return(NA)
    }

    if(p == 1) {
      x <- mm[, keep]
      d <- abs((x - median(x)) / mad(x))
    }
    else
      d <- try(sqrt(fun(mm[, keep, drop = FALSE], distance = TRUE)$dist))
    
    if(class(d) == "try-error") {
      warning("could not compute robust distances")
      return(NA)
    }

    c(p, d)
  }

  dists <- lapply(x, compute.distances)
  idx <- which(sapply(dists, length) != 1 + n.res)
  for(i in idx)
    dists[[idx]] <- rep(NA, 1 + n.res[idx])

  for(i in 1:n.models) {
    dists[[i]][1] <- qchisq(level, df = dists[[i]][1])

    if(is.null(x[[i]]$scale))
      s <- sqrt(sum(res[[i]]^2) / x[[i]]$df.residual)
    else
      s <- x[[i]]$scale

    res[[i]] <- c(s, res[[i]])
  }

  x.range <- max(unlist(dists))
  x.range <- c(-0.025 * x.range, 1.05 * x.range)
  y.range <- 1.05 * max(abs(range(unlist(res))))
  y.range <- c(-y.range, y.range)

  panel.special <- function(x, y, id.n, ...)
  {
    a <- x[1]
    b <- y[1]
    x <- x[-1]
    y <- y[-1]
    if(!is.na(a)) {
      panel.xyplot(x, y, ...)
      #panel.addons(x, y, id.n = id.n)
      panel.abline(v = a, lty = 2)
      panel.abline(h = b, lty = 2)
      panel.abline(h = -b, lty = 2)
    }
    invisible()
  }

  mod <- factor(rep(mod.names, 1 + n.res), levels = mod.names)
  tdf <- data.frame(res = unlist(res), dist = unlist(dists), mod = mod)

  p <- xyplot(res ~ dist | mod,
              data = tdf,
              panel = panel.special,
              xlim = x.range,
              ylim = y.range,
              strip = function(...) strip.default(..., style = 1),
              id.n = id.n,
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


