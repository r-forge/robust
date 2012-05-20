plot.lmfm <- function(x, which.plots = c(5, 2, 6, 4), ...)
{
  choices <- c("All",
    "Normal QQ Plot of Residuals", 
    "Kernel Density Estimate of Residuals", 
    "Residuals vs. Leverage", 
    "Residuals vs. Fitted Values", 
    "Scale-Location", 
    "Response vs. Fitted Values", 
    "Residuals vs. Index (Time)", 
    "Overlaid Normal QQ Plot of Residuals", 
    "Overlaid Kernel Density Estimate of Residuals")

  if(length(attr(x[[1]]$terms, "term.labels")) == 1)
    choices <- c(choices, "Scatter Plot with Overlaid Fit(s)")

  all.plots <- 2:length(choices)

  tmenu <- paste("plot:", choices)

  if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, all.plots)

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

        lmfmResQQPlot(x,
                      main = "Normal QQ Plot of Residuals",
                      xlab = "Standard Normal Quantiles",
                      ylab = "Ordered Residuals",
                      pch = 16,
                      ...),

        lmfmResKernDenPlot(x,
                           main = "Kernel Density Estimate of Residuals",
                           xlab = "Residuals",
                           ylab = "Density",
                           ...),

        lmfmResVsLevPlot(x,
                         main = "Residuals vs. Leverage",
                         xlab = "Leverage",
                         ylab = "Residuals",
                         pch = 16,
                         ...),

        lmfmResVsFittedPlot(x,
                            main = "Residuals vs. Fitted Values",
                            xlab = "Fitted Values",
                            ylab = "Residuals",
                            pch = 16,
                            ...),

        lmfmSqrtResVsFittedPlot(x,
                                main = "Scale-Location",
                                xlab = "Fitted Values",
                                ylab = expression(sqrt(abs(plain(Residuals)))),
                                pch = 16,
                                ...),

        lmfmRespVsFittedPlot(x,
                             main = "Response vs. Fitted Values",
                             xlab = "Fitted Values",
                             ylab = "Response",
                             pch = 16,
                             ...),

        lmfmResVsIdxPlot(x,
                         main = "Residuals vs. Index (Time)",
                         xlab = "Index (Time)",
                         ylab = "Residuals",
                         pch = 16,
                         ...),

        lmfmOverlaidQQPlot(x,
                           main = "Normal QQ Plot of Residuals",
                           xlab = "Quantiles of Standard Normal",
                           ylab = "Ordered Residuals",
                           pch = rep(16, n.models),
                           col = colors,
                           ...),

        lmfmOverlaidResDenPlot(x,
                               main = "Kernel Density Estimate of Residuals",
                               xlab = "Residuals",
                               ylab = "Density",
                               lwd = n.models:1,
                               col = colors,
                               ...),

        lmfm2DRegPlot(x,
                      main = format(formula(x[[1]])),
                      lwd = n.models:1,
                      col = colors,
                      ...)
      )
    }
  }
  invisible(x)
}


