plot.lmfm <- function(x, which.plots = "ask", ...)
{
	choices <- c("All",
		"Normal QQ-Plot of Residuals", 
		"Estimated Kernel Density of Residuals", 
		"Robust Residuals vs Robust Distances", 
		"Residuals vs Fitted Values", 
		"Sqrt of abs(Residuals) vs Fitted Values", 
		"Response vs Fitted Values", 
		"Standardized Residuals vs Index (Time)", 
		"Overlaid Normal QQ-Plot of Residuals", 
		"Overlaid Estimated Density of Residuals")

	if(length(attr(x[[1]]$terms, "term.labels")) == 1) {
		choices <- c(choices, "Scatter Plot with Fits")
		all.plots <- 2:11
  }
  else
    all.plots <- 2:10

	tmenu <- paste("plot:", choices)

	if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, all.plots)
		ask <- FALSE
		which.plots <- c(which.plots + 1, 1)
	}

	else if(which.plots == "all") {
		which.plots <- c(all.plots + 1, 1)
		ask <- FALSE
	}

	else
		ask <- TRUE

	while(TRUE) {
		if(ask) {
			which.plots <- menu(tmenu,
        title = "\nMake plot selections (or 0 to exit):\n")
			if(any(which.plots == 1))
				which.plots <- all.plots
			which.plots <- 1 + which.plots
		}

		graph.number <- 1
		if(dev.cur() == 1 && which.plots[1] != 1)
			trellis.device()
		for(pick in which.plots) {
			switch(pick,
				return(invisible(x)),
				{
					## Place Holder ##
				}
				,
				{
          lmfmResQQPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residuals QQ")

						graph.number <- graph.number + 1
					}
				}

				,
				{
          lmfmResKernDenPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residual Density")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmSRvsRDPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residuals vs. Robust Distances")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmResVsFittedPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residuals vs. Fitted Values")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmSqrtResVsFittedPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Sqrt(abs(Residuals)) vs. Fitted Values")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmRespVsFittedPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Response vs. Fitted Values")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmStdResPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Standardized Residuals vs. Index")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmOverlaidQQPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Overlaid Residuals QQ")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmOverlaidResDenPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Overlaid Residual Density")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfm2DRegPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Scatter Plot")

						graph.number <- graph.number + 1
					}
				}
			)
		}
	}
  invisible(x)
}


