covfmDistancePlot <- function(x, chisq.percent = 0.975, id.n = 3, main, xlab,
                               ylab, ...)
{
	n.models <- length(x)
	mod.names <- names(x)

  if(n.models == 2) {

    p <- dim(x[[1]]$cov)[1]
    n <- length(x[[1]]$dist)

    the.title <- paste(mod.names[1], " Distance vs. ",
                       mod.names[2], " Distance", sep = "")
    x.label <- paste(mod.names[2], "Distance")
    y.label <- paste(mod.names[1], "Distance")

    if(missing(main))
      main <- "Distance-Distance Plot"

    if(missing(xlab))
      xlab <- x.label

    if(missing(ylab))
      ylab <- y.label

    tdf <- data.frame(X2 = sqrt(x[[2]]$dist), 
                      X1 = sqrt(x[[1]]$dist),
                      mod = rep(the.title, n))

    prepanel.special <- function(x, y, threshold = 3) {
      xlim <- c(0.0, max(x, 1.25 * threshold))
      ylim <- c(0.0, max(y, 1.25 * threshold))
      list(xlim = xlim, ylim = ylim)
    }

    panel.special <- function(x, y, threshold = 3, id.n = 3) {
      panel.xyplot(x, y, pch = 16)
      n <- length(y)
      outliers <- which(x > threshold | y > threshold)
      if(id.n > 0) {
        id <- order(x^2 + y^2)[(n-id.n+1):n]
        outliers <- intersect(id, outliers)
        if(length(outliers))
          panel.text(x[outliers], y[outliers], paste(" ", outliers, sep = ""), adj = 0)
      }
      panel.abline(h = threshold, lty = 2)
      panel.abline(v = threshold, lty = 2)
      panel.abline(c(0, 1), lty = 4)
      invisible()
    }

    p <- xyplot(X1 ~ X2 | mod,
                data = tdf,
                main = main,
                xlab = xlab,
                ylab = ylab,
                panel = panel.special,
                prepanel = prepanel.special,
                threshold = sqrt(qchisq(chisq.percent, p)),
                id.n = id.n,
                strip = function(...) strip.default(...,style = 1))

    print(p)
    return(invisible(p))
  }

  invisible(x)
}


