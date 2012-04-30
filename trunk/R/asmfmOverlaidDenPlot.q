asmfmOverlaidDenPlot <- function(x, truncate = 0.999, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- sapply(x, function(u) !is.null(u$data))

  if(!any(data))
    stop("data is not available")

  data <- x[[(1:n.models)[data][1]]]$data

  if(!is.null(truncate)) {
    b <- numeric(n.models)
    for(i in 1:n.models) {
      b[i] <- switch(x[[i]]$distribution,
        gamma = qgamma(truncate, shape = x[[i]]$alpha, scale = x[[i]]$sigma)
      )
    }
    b <- max(b)
    data <- data[data < b]
  }

  prepanel.special <- function(x, fm, ...) {
    n.models <- length(fm)
    pp <- prepanel.default.histogram(x, breaks = "FD")

    for(i in 1:n.models) {
      switch(fm[[i]]$distribution,
        gamma = {
          a <- fm[[i]]$alpha
          s <- fm[[i]]$sigma
          u <- dgamma((a - 1.0) * s, shape = a, scale = s)
          if(u > pp$ylim[2])
            pp$ylim[2] <- u
        }
      )#end switch
    }
    pp
  }

  panel.special <- function(x, fm, col, lwd, lty, ...)
  {
    n.models <- length(fm)

    if(!is.null(col.hist <- list(...)$col.hist))
      panel.histogram(x, breaks = "FD", col = col.hist)
    else
      panel.histogram(x, breaks = "FD")

    if(missing(col))
      col <- 1:n.models

    if(missing(lwd))
      lwd <- 1:n.models

    if(missing(lty))
      lty <- 1:n.models

    col <- rep(col, n.models)
    lwd <- rep(lwd, n.models)
    lty <- rep(lty, n.models)

    for(i in 1:n.models) {
      switch(fm[[i]]$distribution,
        gamma = {
          dist.fun <- dgamma
          dist.args <- list(shape = fm[[i]]$alpha, scale = fm[[i]]$sigma)
        },
        lognormal = {
          dist.fun <- dlnorm
          dist.args <- list()
        },
        weibull = {
          dist.fun <- dweibull
          dist.args <- list()
        }
      )

      panel.mathdensity(dmath = dist.fun,
                        args = dist.args,
                        n = 250,
                        col = col[i],
                        lwd = lwd[i],
                        lty = lty[i],
                        ...)
    }

    invisible()
  }

  key <- simpleKey(corner = c(0.95, 0.95),
                   text = mod.names,
                   points = FALSE,
                   lines = TRUE)
  key$lines$col <- if(!is.null(col <- list(...)$col)) col else 1:n.models
  key$lines$lwd <- if(!is.null(lwd <- list(...)$lwd)) lwd else 1:n.models
  key$lines$lty <- if(!is.null(lty <- list(...)$lty)) lty else 1:n.models

  p <- histogram(~ data | "",
                 type = "density",
                 prepanel = prepanel.special,
                 panel = panel.special,
                 strip = function(...) strip.default(..., style = 1),
                 fm = x,
                 key = key,
                 ...)

  print(p)
  invisible(p)
}


