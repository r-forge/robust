plot.covfm <- function(x, which.plots = "ask", ...)
{
	n.models <- length(x)

	if(n.models == 2)
		choices <- c("All",
                 "Eigenvalues of Covariance Estimate", 
                 "Sqrt of Mahalanobis Distances",
                 "Ellipses Plot",
                 "Distance - Distance Plot")

	else
		choices <- c("All",
                 "Eigenvalues of Covariance Estimate", 
                 "Sqrt of Mahalanobis Distances",
                 "Ellipses Plot")

  all.plots <- 2:length(choices)

	tmenu <- paste("plot:", choices)

	if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, all.plots)

    if(!length(which.plots))
      return(invisible(x))

    if(length(which.plots) > 1) {
      par.ask <- par(ask = TRUE)
      on.exit(par(ask = par.ask))
    }

		ask <- FALSE
		which.plots <- c(which.plots, 0)
	}

	else if(which.plots == "all") {
		which.plots <- c(all.plots, 0)
		ask <- FALSE
    par.ask <- par(ask = TRUE)
    on.exit(par(ask = par.ask))
	}

	else
		ask <- TRUE

	repeat {
		if(ask)
			which.plots <- menu(tmenu, title = "\nMake plot selections (or 0 to exit):\n")

    which.plots <- intersect(which.plots, c(0, all.plots))

    if(!length(which.plots))
      stop(paste("Invalid choice of plot in \'which.plots\'"))

    which.plots <- which.plots + 1

    for(i in 1:length(which.plots)) {
      pick <- which.plots[i]
      switch(pick,
        return(invisible(x)),
        place.holder <- 1,
        covfmScreePlot(x, ...),
        covfmSqrtMDPlot(x, ...),
        covfmEllipsesPlot(x, ...),
        covfmDistancePlot(x, ...),
			)

		}
	}
	invisible(x)
}

