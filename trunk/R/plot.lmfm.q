plot.lmfm <- function(x, which.plots = ifelse(interactive(), "ask", "all"),
                      ...)
{
  choices <- c("All",
    "Normal QQ Plot of Residuals", 
    "Kernel Density Estimate of Residuals", 
    "Residuals vs. Robust Distances", 
    "Residuals vs. Fitted Values", 
    "Square Root of Absolute Residuals vs. Fitted Values", 
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
        lmfmResQQPlot(x, ...),
        lmfmResKernDenPlot(x, ...),
        lmfmResVsRDPlot(x, ...),
        lmfmResVsFittedPlot(x, ...),
        lmfmSqrtResVsFittedPlot(x, ...),
        lmfmRespVsFittedPlot(x, ...),
        lmfmResVsIdxPlot(x, ...),
        lmfmOverlaidQQPlot(x, ...),
        lmfmOverlaidResDenPlot(x, ...),
        lmfm2DRegPlot(x, ...)
      )
    }
  }
  invisible(x)
}


