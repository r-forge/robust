plot.glmfm <- function(x, which.plots = c(2, 5, 7, 6), ...)
{
  n.models <- length(x)

  dev.res.fun <- function(u)
    residuals(u, type = "deviance")

  pear.res.fun <- function(u)
    residuals(u, type = "pearson")

  choices <- c("All",
               "Deviance Residuals vs. Fitted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Pearson Residuals",
               "QQ Plot of Deviance Residuals",
               "Deviance Residuals vs. Design Distance",
               "Scale-Location")

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
                            residuals.fun = dev.res.fun,
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
                      residuals.fun = pear.res.fun,
                      xlab = "Standard Normal Quantiles",
                      ylab = "Empirical Quantiles of Pearson Residuals",
                      main = "Normal QQ Plot of Pearson Residuals",
                      envelope = FALSE,
                      pch = 16,
                      ...),

        glmfmResQQPlot(x,
                      residuals.fun = dev.res.fun,
                      xlab = "Theoretical Quantiles",
                      ylab = "Ordered Deviance Residuals",
                      main = "QQ Plot of Deviance Residuals",
                      pch = 16,
                      ...),

        lmfmResVsDistPlot(x,
                          residuals.fun = dev.res.fun,
                          xlab = "Design Distance",
                          ylab = "Deviance Residuals",
                          main = "Deviance Residuals vs. Design Distance",
                          pch = 16,
                          ...),

        lmfmSqrtResVsFittedPlot(x,
                                residuals.fun = dev.res.fun,
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


