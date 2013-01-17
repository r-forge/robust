plot.glmfm <- function(x, which.plots = c(2, 5, 7, 6), ...)
{
  n.models <- length(x)

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

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) residuals(u, type = "deviance"),
                         xlab = "Fitted Values",
                         ylab = "Deviance Residuals",
                         main = "Deviance Residuals vs. Fitted Values",
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) model.response(model.frame(u)),
                         xlab = "Fitted Values",
                         ylab = "Response",
                         main = "Response vs. Fitted Values",
                         ...),

        qqPlot.lmfm(x,
                    fun = function(u) residuals(u, type = "pearson"),
                    xlab = "Standard Normal Quantiles",
                    ylab = "Empirical Quantiles of Pearson Residuals",
                    main = "Normal QQ Plot of Pearson Residuals",
                    envelope = FALSE,
                    ...),

        glmfmResQQPlot(x,
                      residuals.fun = function(u) residuals(u, type = "deviance"),
                      xlab = "Theoretical Quantiles",
                      ylab = "Ordered Deviance Residuals",
                      main = "QQ Plot of Deviance Residuals",
                      ...),

        scatterPlot.lmfm(x,
                         x.fun = leverage,
                         y.fun = function(u) residuals(u, type = "deviance"),
                         xlab = "Leverage",
                         ylab = "Deviance Residuals",
                         main = "Deviance Residuals vs. Leverage",
                         ...),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) sqrt(abs(residuals(u, type = "deviance"))),
                         xlab = "Fitted Values",
                         ylab = expression(sqrt(abs(plain("Deviance Residuals")))),
                         main = "Scale-Location",
                         ...)

      ) ## switch(pick, ..)
    } ## end for(...)
  } ## repeat {...}

  invisible(x)
}


