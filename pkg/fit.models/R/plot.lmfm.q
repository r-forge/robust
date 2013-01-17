plot.lmfm <- function(x, which.plots = c(5, 2, 6, 4), ...)
{
  choices <- c("All",
    "Normal QQ Plot of Modified Residuals", 
    "Kernel Density Estimate of Modified Residuals", 
    "Modified Residuals vs. sqrt(Leverage)", 
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
                    main = "Normal QQ Plot of Modified Residuals",
                    xlab = "Standard Normal Quantiles",
                    ylab = "Empirical Quantiles of Modified Residuals",
                    ...),

        kernDenPlot.lmfm(x,
                         fun = rmodified,
                         main = "Kernel Density Estimate of Modified Residuals",
                         xlab = "Modified Residuals",
                         ylab = "Density",
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = function(u) sqrt(leverage(u)),
                         y.fun = rmodified,
                         xlab = expression(sqrt(plain("Leverage"))),
                         ylab = "Modified Residuals",
                         main = expression(paste(plain("Modified Residuals vs. "),
                                                 sqrt(plain("Leverage")))),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = resid,
                         main = "Residuals vs. Fitted Values",
                         xlab = "Fitted Values",
                         ylab = "Residuals",
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) sqrt(abs(rmodified(u))),
                         main = "Scale-Location",
                         xlab = "Fitted Values",
                         ylab = expression(sqrt(abs(plain("Modified Residuals")))),
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) model.response(model.frame(u)),
                         main = "Response vs. Fitted Values",
                         xlab = "Fitted Values",
                         ylab = "Response",
                         ...),

        indexPlot.lmfm(x,
                       fun = rmodified,
                       main = "Modified Residuals vs. Index (Time)",
                       xlab = "Index (Time)",
                       ylab = "Modified Residuals",
                       ...),

        overlaidQQPlot.lmfm(x,
                            fun = rmodified,
                            main = "Normal QQ Plot of Modified Residuals",
                            xlab = "Standard Normal Quantiles",
                            ylab = "Empirical Quantiles of Modified Residuals",
                            ...),

        overlaidKernDenPlot.lmfm(x,
                                 fun = rmodified,
                                 main = "Kernel Density Estimate of Modified Residuals",
                                 xlab = "Modified Residuals",
                                 ylab = "Density",
                                 ...),

        simpleRegPlot.lmfm(x,
                           main = "Scatter Plot with Overlaid Fits",
                           lwd.reg = n.models:1,
                           col.reg = colors,
                           ...)
      )
    }
  }

  invisible(x)
}





