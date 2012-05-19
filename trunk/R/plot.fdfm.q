plot.fdfm <- function(x, which.plots = ifelse(interactive(), "ask", "all"), ...)
{
  data.name <- attributes(x)$data.name

  choices <- c("All",
               "Overlaid Density Estimates", 
               "Sample QQ Plot")

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
        
        fdfmOverlaidDenPlot(x,
                            main = "Overlaid Density Estimates",
                            xlab = data.name,
                            col = colors,
                            col.hist = "lightgray",
                            lwd = n.models:1,
                            lty = 1:n.models,
                            ...),

        fdfmQQPlot(x,
                   main = "Sample QQ Plot",
                   xlab = "Estimated Quantiles",
                   ylab = "Ordered Sample",
                   pch = 16,
                   ...)

      )
    }
  }

  invisible(x)
}


