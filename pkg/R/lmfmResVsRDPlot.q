lmfmResVsRDPlot <- function(x, type = "response", level = 0.95, id.n = 3,
                            main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Residuals vs. Robust Distances"

  if(missing(xlab))
    xlab <- "Robust Distances"

  if(missing(ylab))
    ylab <- "Residuals"

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))

  resids <- sapply(x, resid, type = type)

  model <- sapply(x, function(u) !is.null(u$model))
  if(!any(model))
    stop("none of the fitted models in ", sQuote(deparse(substitute(x))),
         "contain a model frame component")
  model <- x[[(1:n.models)[model][1]]]$model
  model <- model[sapply(model, is.numeric)]

  if(length(model)) {

    p <- dim(model)[2]
    dist <- sqrt(covRob(model, distance = TRUE)$dist)

    res.thresh <- qnorm(level)
    dist.thresh <- qchisq(level, df = p)

    y.range <- range(resids)
    y.range[1] <- 1.05 * min(y.range[1], -res.thresh)
    y.range[2] <- 1.05 * max(y.range[2], res.thresh)

    x.range <- c(0.0, max(dist))
    x.range[2] <- 1.05 * max(x.range[2], res.thresh)

    panel.special <- function(x, y, res.thresh = 1.0, dist.thresh = 1.0, id.n = 3)
    {
      panel.xyplot(x, y, col = 6, pch = 16)
      panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)
      panel.abline(v = dist.thresh, lty = 2)
      panel.abline(h = res.thresh, lty = 2)
      panel.abline(h = -res.thresh, lty = 2)
      invisible()
    }

    mod <- factor(rep(mod.names, each = n), levels = mod.names)

    df <- data.frame(RD = rep(dist, n.models),
                     RR = as.vector(resids),
                     mod = mod)

    p <- xyplot(RR ~ RD | mod,
                data = df,
                xlab = xlab,
                panel = panel.special,
                xlim = x.range,
                ylim = y.range,
                ylab = ylab,
                main = main,
                strip = function(...) strip.default(..., style = 1),
                res.thresh = res.thresh,
                dist.thresh = dist.thresh,
                id.n = id.n,
                layout = c(n.models, 1, 1),
                ...)

    print(p)
  }

  else {
    warning("robust distances could not be computed because there are no numeric variables in the model frame")
    p <- NA
  }

  invisible(p)
}


