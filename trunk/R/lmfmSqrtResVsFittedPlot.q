lmfmSqrtResVsFittedPlot <- function(x, type = "response", smooths = FALSE,
  rugplot = FALSE, id.n = 3, main, xlab, ylab, ...)
{
  if(missing(main))
    main <- "Square Root of Absolute Residuals vs. Fitted Values"

  if(missing(xlab))
    xlab <- "Fitted Values"

  if(missing(ylab))
    ylab <- expression(sqrt(abs(plain(Residuals))))

  n.models <- length(x)
  mod.names <- names(x)
  n <- length(resid(x[[1]]))

  panel.special <- function(x, y, smooths = FALSE, rugplot = FALSE, id.n = 3)
  {
    panel.xyplot(x, y, pch = 16, col = 6)
    panel.addons(x, y, smooths = smooths,
      rugplot = rugplot, id.n = id.n)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(f = as.vector(sapply(x, fitted)),
                   r = as.vector(sqrt(abs(sapply(x, resid, type = type)))),
                   mod = mod)

  p <- xyplot(r ~ f | mod,
              data = df,
              xlab = xlab,
              ylab = ylab,
              main = main,
              panel = panel.special,
              smooths = smooths,
              rugplot = rugplot,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


