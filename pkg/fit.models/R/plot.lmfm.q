plot.lmfm <- function(x, which.plots = c(5, 2, 6, 4), ...)
{
  choices <- c("All",
    "Normal QQ Plot of Modified Residuals", 
    "Kernel Density Estimate of Modified Residuals",
    "Modified Residuals vs. Leverage",
    "Modified Residuals vs. Fitted Values", 
    "Scale-Location", 
    "Response vs. Fitted Values", 
    "Modified Residuals vs. Index (Time)", 
    "Overlaid Normal QQ Plot of Modified Residuals", 
    "Overlaid Kernel Density Estimate of Modified Residuals")

  if(length(attr(x[[1]]$terms, "term.labels")) == 1)
    choices <- c(choices, "Scatter Plot with Overlaid Fit(s)")

  all.plots <- 2:length(choices)

  tmenu <- paste("plot:", choices)

  if(is.numeric(which.plots)) {
    if(!all(which.plots %in% all.plots))
      stop(sQuote("which"), " must be in 2:", length(choices))

    if(length(which.plots) == 0)
      return(invisible(x))

    if(length(which.plots) > 1) {
      par.ask <- par(ask = TRUE)
      on.exit(par(ask = par.ask))
    }

    ask <- FALSE
    which.plots <- c(which.plots + 1, 1)
  }

  else if(which.plots == "all") {
    which.plots <- c(all.plots + 1, 1)
    ask <- FALSE
    par.ask <- par(ask = TRUE)
    on.exit(par(ask = par.ask))
  }

  else
    ask <- TRUE

  n.models <- length(x)
  if(n.models <= 3)
    colors <- c("black", "blue", "purple")[1:n.models]
  else
    colors <- 1:n.models

  repeat {
    if(ask) {
      which.plots <- menu(tmenu,
        title = "\nMake plot selections (or 0 to exit):\n")

      if(any(which.plots == 1)) {
        which.plots <- c(all.plots, 0)
        par.ask <- par(ask = TRUE)
        on.exit(par(ask = par.ask))
      }

      which.plots <- which.plots + 1
    }

    for(pick in which.plots) {
      switch(pick,
        return(invisible(x)),

        place.holder <- 1,

        qqPlot.lmfm(x,
                    fun = rmodified,
                    main = expression(plain("Normal QQ Plot of Modified Residuals")),
                    xlab = expression(plain("Standard Normal Quantiles")),
                    ylab = expression(plain("Empirical Quantiles of Modified Residuals")),
                    ...),

        kernDenPlot.lmfm(x,
                         fun = rmodified,
                         main = expression(plain("Kernel Density Estimate of Modified Residuals")),
                         xlab = expression(plain("Modified Residuals")),
                         ylab = expression(plain("Density")),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = leverage,
                         y.fun = rmodified,
                         xlab = expression(plain("Leverage")),
                         ylab = expression(plain("Modified Residuals")),
                         main = expression(plain("Modified Residuals vs. Leverage")),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = residuals,
                         main = expression(plain("Residuals vs. Fitted Values")),
                         xlab = expression(plain("Fitted Values")),
                         ylab = expression(plain("Residuals")),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) sqrt(abs(rmodified(u))),
                         main = expression(plain("Scale-Location")),
                         xlab = expression(plain("Fitted Values")),
                         ylab = expression(sqrt(abs(plain("Modified Residuals")))),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) model.response(model.frame(u)),
                         main = expression(plain("Response vs. Fitted Values")),
                         xlab = expression(plain("Fitted Values")),
                         ylab = expression(plain("Response")),
                         ...),

        indexPlot.lmfm(x,
                       fun = rmodified,
                       main = expression(plain("Modified Residuals vs. Index (Time)")),
                       xlab = expression(plain("Index (Time)")),
                       ylab = expression(plain("Modified Residuals")),
                       ...),

        overlaidQQPlot.lmfm(x,
                            fun = rmodified,
                            main = expression(plain("Normal QQ Plot of Modified Residuals")),
                            xlab = expression(plain("Standard Normal Quantiles")),
                            ylab = expression(plain("Empirical Quantiles of Modified Residuals")),
                            ...),

        overlaidKernDenPlot.lmfm(x,
                                 fun = rmodified,
                                 main = expression(plain("Kernel Density Estimate of Modified Residuals")),
                                 xlab = expression(plain("Modified Residuals")),
                                 ylab = expression(plain("Density")),
                                 ...),

        simpleRegPlot.lmfm(x,
                           main = expression(plain("Scatter Plot with Overlaid Fits")),
                           lwd.reg = n.models:1,
                           col.reg = colors,
                           ...)
      )
    }
  }

  invisible(x)
}





