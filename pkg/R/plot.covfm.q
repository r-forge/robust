plot.covfm <- function(x, which.plots = "ask", ...)
{
	n.models <- length(x)
	mod.names <- names(x)

  ## menu choices ##

	if(n.models == 2)
		choices <- c(	"All",
									"Eigenvalues of Covariance Estimate", 
									"Sqrt of Mahalanobis Distances",
									"Ellipses Plot",
									"Distance - Distance Plot")

	else
		choices <- c(	"All",
									"Eigenvalues of Covariance Estimate", 
									"Sqrt of Mahalanobis Distances",
									"Ellipses Plot")

	tmenu <- paste("plot:", choices)

	ask <- TRUE

	if(is.numeric(which.plots)) {
		ask <- FALSE
		which.plots <- c(which.plots, 0)
	}

	else if(which.plots == "all")
		which.plots <- c(3:6, 1)

	while(TRUE) {
		if(ask)
			which.plots <- menu(tmenu, title = "\nMake plot selections (or 0 to exit):\n")

    if(any(which.plots == 1)) {
      if(n.models == 2)
        which.plots <- 2:5
      else
        which.plots <- 2:4
      which.plots <- c(which.plots, 0)
    }	

    if(n.models == 2)
      which.plots <- intersect(which.plots, c(0, 2:5))
    else
      which.plots <- intersect(which.plots, c(0, 2:4))

    if(!length(which.plots))
      stop(paste("Invalid choice of plot in \'which.plots\'"))

    which.plots <- which.plots + 1
    tab.index <- 1

    if(dev.cur() == 1 && which.plots[1] != 1)
      trellis.device()

    for(i in 1:length(which.plots)) {
      pick <- which.plots[i]
      switch(pick,
        {
        return(invisible(x))
        }
        ,
        {
          ## Place Holder ##
        }
        ,
        {
          covfmScreePlot(x, ...)

          if(names(dev.cur()) == "graphsheet") {
            guiModify("GraphSheetPage",
              Name = paste("$", tab.index, sep = ""),
              NewName = "Screeplot")

            tab.index <- tab.index + 1
          }
        }
        ,
        {
          covfmSqrtMDPlot(x, ...)

          if(names(dev.cur()) == "graphsheet") {
            guiModify("GraphSheetPage",
              Name = paste("$", tab.index, sep = ""),
              NewName = "Robust Distances")

            tab.index <- tab.index + 1
          }
        }
        ,
        {
          covfmEllipsesPlot(x, ...)

          if(names(dev.cur()) == "graphsheet") {
            guiModify("GraphSheetPage",
              Name = paste("$", tab.index, sep = ""),
              NewName = "Ellipses Matrix")

            tab.index <- tab.index + 1
          }
        }
        ,
        {
          covfmDistance2Plot(x, ...)

          if(names(dev.cur()) == "graphsheet") {
            guiModify("GraphSheetPage",
              Name = paste("$", tab.index, sep = ""),
              NewName = "Distance - Distance")

            tab.index <- tab.index + 1
          }
        }
			) #end of switch
		}
	}
	invisible(x)
}

