lmfmRespVsFittedPlot <- function(x, smooths = FALSE, rugplot = FALSE, main,
                                 xlab, ylab, ...)
{
  if(missing(main))
    main <- "Response vs. Fitted Values"

  if(missing(xlab))
    xlab <- "Fitted Values"

  if(missing(ylab))
    ylab <- "Response"

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))

  model <- sapply(x, function(u) !is.null(u$model))
  if(!any(model))
    stop("none of the fitted models in ", sQuote(deparse(substitute(x))),
         "contain a model frame component")
  model <- x[[(1:n.models)[model][1]]]$model
  y <- model.extract(model, "response")

  panel.special <- function(x, y, smooths = FALSE, rugplot = FALSE)
  {
    panel.xyplot(x, y, col = 6, pch = 16)
    panel.addons(x, y, smooths = smooths, rugplot = rugplot, id.n = 0)
    panel.abline(0, 1, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(y = rep(y, n.models),
                   fv = as.vector(sapply(x, fitted)),
                   mod = mod)

  p <- xyplot(y ~ fv | mod,
              data = df,
              xlab = xlab,
              ylab = ylab,
              main = main,
              panel = panel.special,
              smooths = smooths,
              rugplot = rugplot,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


