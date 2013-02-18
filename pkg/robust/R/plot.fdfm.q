plot.fdfm <- function(x, which.plots = 2:3, ...)
{
  choices <- c("All",
               "Overlaid Density Estimates", 
               "Sample QQ Plot")

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
  data.name <- x[[1]]$data.name

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
        
        overlaidDenPlot.fdfm(x,
                             main = "Overlaid Density Estimates",
                             xlab = data.name,
                             ...),

        qqPlot.fdfm(x,
                    main = "Sample QQ Plot",
                    xlab = "Theoretical Quantiles",
                    ylab = paste("Empirical Quantiles of", data.name),
                    ...)

      )
    }
  }

  invisible(x)
}


