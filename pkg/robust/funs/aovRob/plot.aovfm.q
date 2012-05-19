plot.aovfm <- function(x, which.plots = "ask", ...)
{
	choices <- c("All",
		"Normal QQ-Plot of Residuals", 
		"Estimated Kernel Density of Residuals", 
		"Residuals vs Fitted Values", 
		"Sqrt of abs(Residuals) vs Fitted Values", 
		"Response vs Fitted Values", 
		"Residual-Fit Spread",
		"Standardized Residuals vs Index (Time)",
		"Overlaid Normal QQ-Plot of Residuals", 
		"Overlaid Estimated Density of Residuals")
	tmenu <- paste("plot:", choices)

	ask <- TRUE

	if(is.integer(which.plots)) {
		ask <- FALSE
		which.plots <- c(which.plots + 1, 1)
	}

	else if(which.plots == "all") {
		ask <- FALSE
		which.plots <- c(3:11, 1)
	}

	while(TRUE) {
		if(ask) {
			which.plots <- menu(tmenu, title = 
				"\nMake plot selections (or 0 to exit):\n")
			if(any(which.plots == 1))
				which.plots <- 2:10
				which.plots <- 1 + which.plots
			}

		graph.number <- 1
		if(dev.cur() == 1 && which.plots[1] != 1)
			trellis.device()
		for(iwhich in 1:length(which.plots)) {
			pick <- which.plots[iwhich]
			switch(pick,
				invisible(return(x)),
				{
					ask.now <- F
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
							NewName = "Kernel Density of Residuals")

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
							NewName = "sqrt(abs(Residuals)) vs. Fitted Values")

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
					resfit <- numeric()
					title <- character()
					frac <- numeric()

					for(i in 1:n.models) {
						resfit <- c(resfit, sort(x[[i]]$fitted - mean(x[[i]]$fitted)))
						resfit <- c(resfit, sort(x[[i]]$residuals))
						
						data.len <- length(x[[i]]$fitted)
						
						title <- c(title, c(rep(paste(mod.names[i], " Fitted", sep = ":"), data.len),
							rep(paste(mod.names[i], " Residuals", sep = ":"), data.len)))
							
						frac <- c(frac, rep((1:data.len)/data.len,2))
					}

					print(xyplot(resfit ~ frac | as.factor(title),
						pch = 16,
						col = 6,
						main = "Residual-Fit Spread",
						xlab = "f-value",
						ylab = "",
						between = list(x = 0, y = 1),	
						strip = function(...) strip.default(..., style = 1)))

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residual-Fit Spread")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmStdResPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Residuals vs. Index")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmOverlaidQQPlot(x, ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "QQ-Plot of Residuals")

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
			)
		}
	}

	invisible(x)
}


