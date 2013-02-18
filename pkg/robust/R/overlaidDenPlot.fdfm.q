overlaidDenPlot.fdfm <- function(x, trunc = 1.0 - 1e-3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  data <- x[[1]]$x

  if(!is.null(trunc)) {
    b <- numeric(n.models)
    for(i in 1:n.models)
      b[i] <- do.call(paste("q", x[[i]]$densfun, sep = ""),
                      c(list(p = trunc), x[[i]]$estimate))
    b <- min(b)
    data <- data[data < b]

## maybe add warning or message here ##

  }

  prepanel.special <- function(x, fm, ...) {
    n.models <- length(fm)
    pp <- prepanel.default.histogram(x, breaks = "FD")

    for(i in 1:n.models) {

      left <- do.call(paste("d", fm[[i]]$densfun, sep = ""),
                      c(list(x = 0.0), fm[[i]]$estimate))

      if(left < 1e-6) {
        fun <- function(u) -1.0 * do.call(paste("d", fm[[i]]$densfun, sep = ""),
                                          c(list(x = u), fm[[i]]$estimate))
        opt <- nlm(fun, p = 1.0)

        if(-opt$minimum > pp$ylim[2])
          pp$ylim[2] <- -opt$minimum
      }
    }

    pp
  }

  panel.special <- function(x, fm, ...)
  {
    n.models <- length(fm)
    panel.histogram(x, breaks = "FD", col = "lightgray")

    for(i in 1:n.models) {
      fun <- get(paste("d", fm[[i]]$densfun, sep = ""))
      fun.args <- as.list(fm[[i]]$estimate)

      panel.mathdensity(dmath = fun,
                        args = fun.args,
                        n = 250,
                        col = i,
                        lwd = 1 + n.models - i,
                        lty = i,
                        ...)
    }

    invisible()
  }

  key <- simpleKey(corner = c(0.95, 0.95),
                   text = mod.names,
                   points = FALSE,
                   lines = TRUE)

  key$lines$col <- 1:n.models
  key$lines$lwd <- n.models:1
  key$lines$lty <- 1:n.models

  p <- histogram(~ data | "",
                 panel = panel.special,
                 prepanel = prepanel.special,
                 type = "density",
                 strip = function(...) strip.default(..., style = 1),
                 key = key,
                 fm = x,
                 ...)

  print(p)
  invisible(p)
}


