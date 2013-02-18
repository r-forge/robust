ddPlot.covfm <- function(x, level = 0.95, strip = "", id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(n.models == 2) {
    p <- sapply(x, function(u) dim(u$cov)[1])
    n <- length(x[[1]]$dist)

    tdf <- data.frame(x = sqrt(x[[2]]$dist), 
                      y = sqrt(x[[1]]$dist))

    prepanel.special <- function(x, y, thresh) {
      lim <- c(0.0, max(c(x, y, 1.25 * thresh)))
      list(xlim = lim, ylim = lim)
    }

    panel.special <- function(x, y, thresh, id.n, ...) {
      panel.xyplot(x, y, ...)

      if(id.n > 0) {
        out <- which(x > thresh[1] | y > thresh[2])
        id.n <- ifelse(length(out) > id.n, id.n, length(out))
        out <- out[order(x[out]^2 + y[out]^2, decreasing = TRUE)][1:id.n]

        if(length(out))
          panel.text(x[out], y[out], paste(" ", out, sep = ""), adj = 0)
      }

      panel.abline(h = thresh[2], lty = 2)
      panel.abline(v = thresh[1], lty = 2)
      panel.abline(c(0, 1), lty = 4)
      invisible()
    }

    p <- xyplot(y ~ x | strip,
                data = tdf,
                aspect = "iso",
                panel = panel.special,
                prepanel = prepanel.special,
                thresh = sqrt(qchisq(level, p)),
                id.n = id.n,
                strip = function(...) strip.default(...,style = 1),
                ...)

    print(p)
    return(invisible(p))
  }

  invisible(x)
}


