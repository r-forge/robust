lmfmRespVsFittedPlot <- function(x, smooths = FALSE, rugplot = FALSE, ...)
{
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
    
  df <- data.frame(y = rep(y, n.models),
    fv = as.vector(sapply(x, predict)),
    mod = rep(mod.names, each = n))
      
  print(xyplot(y ~ fv | mod,
    data = df,
    xlab = "Fitted Values",
    ylab = "Response",
    main = "Response vs. Fitted Values",
    panel = panel.special,
    smooths = smooths,
    rugplot = rugplot,
    strip = function(...) strip.default(..., style = 1),
    layout = c(n.models, 1, 1),
    ...))

  invisible(x)
}


