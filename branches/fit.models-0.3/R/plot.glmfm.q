plot.glmfm <- function(x, which.plots = c(2, 5, 8, 6), ...)
{
  n.models <- length(x)

  choices <- c("All",
               "Deviance Residuals vs. Fitted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Pearson Residuals",
               "QQ Plot of Deviance Residuals",
               "Deviance Residuals vs. Leverage",
               "Deviance Residuals vs. Index (Time)",
               "Scale-Location")

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

        lmfmResVsFittedPlot(x,
                            type = "deviance",
                            xlab = "Fitted Values",
                            ylab = "Deviance Residuals",
                            main = "Deviance Residuals vs. Fitted Values",
                            pch = 16,
                            ...),

        lmfmRespVsFittedPlot(x, 
                             xlab = "Fitted Values",
                             ylab = "Response",
                             main = "Response vs. Fitted Values",
                             pch = 16,
                             ...),

        lmfmResQQPlot(x,
                      type = "pearson",
                      xlab = "Standard Normal Quantiles",
                      ylab = "Ordered Pearson Residuals",
                      main = "Normal QQ Plot of Pearson Residuals",
                      envelope = FALSE,
                      pch = 16,
                      ...),

        glmfmResQQPlot(x,
                      type = "deviance",
                      xlab = "Theoretical Quantiles",
                      ylab = "Ordered Deviance Residuals",
                      main = "QQ Plot of Deviance Residuals",
                      pch = 16,
                      ...),

        lmfmResVsLevPlot(x,
                         type = "deviance",
                         xlab = "Leverage",
                         ylab = "Deviance Residuals",
                         main = "Deviance Residuals vs. Leverage",
                         pch = 16,
                         ...),

        lmfmResVsIdxPlot(x,
                         type = "deviance",
                         xlab = "Index (Time)",
                         ylab = "Deviance Residuals",
                         main = "Deviance Residuals vs. Index (Time)",
                         pch = 16,
                         ...),

        lmfmSqrtResVsFittedPlot(x,
                                type = "deviance",
                                xlab = "Fitted Values",
                                ylab = expression(sqrt(abs(plain("Deviance Residuals")))),
                                main = "Scale-Location",
                                pch = 16,
                                ...)
      ) ## switch(pick, ..)
    } ## end for(...)
  } ## repeat {...}
  invisible(x)
}


