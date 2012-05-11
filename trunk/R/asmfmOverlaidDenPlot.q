asmfmOverlaidDenPlot <- function(x, truncate = 0.99, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- attributes(x)$x

  if(!is.null(truncate)) {
    b <- numeric(n.models)
    for(i in 1:n.models) {
      b[i] <- switch(attributes(x)$distribution,
        gamma = {
          do.call("qgamma", as.list(c(p = truncate, x[[i]]$estimate)))
        },
        lognormal = {
          do.call("qlnorm", as.list(c(p = truncate, x[[i]]$estimate)))
        },
        weibull = 1
      )
    }
    b <- max(b)
    data <- data[data < b]
  }

  prepanel.special <- function(x, fm, ...) {
    n.models <- length(fm)
    pp <- prepanel.default.histogram(x, breaks = "FD")

    for(i in 1:n.models) {
      u <- switch(attributes(fm)$distribution,
        gamma = {
          est <- fm[[i]]$estimate
          a <- est[[1]]
          s <- est[[2]]
          m <- ifelse(names(est)[2] == "scale", (a - 1.0) * s, (a - 1.0) / s)
          do.call("dgamma", as.list(c(x = m, est)))
        },
        lognormal = {
          est <- fm[[i]]$estimate
          m <- est[[1]]
          s <- est[[2]]
          do.call("dlnorm", as.list(c(x = exp(m - s^2), est)))
        }
      )#end switch

      if(u > pp$ylim[2])
        pp$ylim[2] <- u
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
      den.fun <- switch(attributes(fm)$distribution,
        gamma = dgamma,
        lognormal = dlnorm,
        weibull = dweibull
      )

      den.args <- as.list(fm[[i]]$estimate)

      panel.mathdensity(dmath = den.fun,
                        args = den.args,
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


