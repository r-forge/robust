plot.glmfm <- function(x, which.plots = c(2, 5, 7, 6), ...)
{
  ## n.models <- length(x)

  choices <- c("All",
               "Deviance Residuals vs. Predicted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Modified Pearson Residuals",
               "Normal QQ Plot of Modified Deviance Residuals",
               "Pearson Residuals vs. Leverage",
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

        place.holder = 1,

        scatterPlot.lmfm(x,
                         x.fun = predict,
                         y.fun = function(u) residuals(u, type = "deviance"),
                         xlab = expression(plain("Predicted Values")),
                         ylab = expression(plain("Deviance Residuals")),
                         main = expression(plain("Deviance Residuals vs. Predicted Values")),
                         pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) model.response(model.frame(u)),
                         xlab = expression(plain("Fitted Values")),
                         ylab = expression(plain("Response")),
                         main = expression(plain("Response vs. Fitted Values")),
                         pch = 16),

        qqPlot.lmfm(x,
                    fun = function(u) rmodified(u, type = "pearson"),
                    xlab = expression(plain("Standard Normal Quantiles")),
                    ylab = expression(plain("Empirical Quantiles of Modified Pearson Residuals")),
                    main = expression(plain("Normal QQ Plot of Modified Pearson Residuals")),
                    envelope = FALSE,
                    pch = 16),

        qqPlot.lmfm(x,
                    fun = function(u) rmodified(u, type = "deviance"),
                    xlab = expression(plain("Standard Normal Quantiles")),
                    ylab = expression(plain("Empirical Quantiles of Modified Deviance Residuals")),
                    main = expression(plain("Normal QQ Plot of Modified Deviance Residuals")),
                    envelope = FALSE,
                    pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = leverage,
                         y.fun = function(v) rmodified(v, type = "pearson"),
                         xlab = expression(plain("Leverage")),
                         ylab = expression(plain("Modified Pearson Residuals")),
                         main = expression(plain("Modified Pearson Residuals vs. Leverage")),
                         pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = predict,
                         y.fun = function(u) sqrt(abs(rmodified(u, type = "deviance"))),
                         xlab = expression(plain("Predicted Values")),
                         ylab = expression(sqrt(abs(plain("Modified Deviance Residuals")))),
                         main = expression(plain("Scale-Location")),
                         pch = 16)
      )
    }
  }

  invisible(x)
}


