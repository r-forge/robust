plot.glmfm <- function(x, which.plots = "ask",	...)
{
	n.models <- length(x)

  ## menu choices ##

	choices <- c("All",
		"Deviance Residuals vs. Fitted Values",
		"Response vs. Fitted Values",
		"Normal QQ Plot of Pearson Residuals",
		"QQ Plot of Deviance Residuals",
		"Standardized Deviance Residuals vs. Robust Distances",
		"Standardized Deviance Residuals vs. Index (Time)",
		"Sqrt of abs(Deviance Residuals) vs. Fitted Values")

	tmenu <- paste("plot:", choices)

	if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, 2:8)
		ask <- FALSE
		which.plots <- c(which.plots + 1, 1)
	}

	else if(which.plots == "all") {
		which.plots <- c(2:8 + 1, 1)
		ask <- FALSE
	}

	else
		ask <- TRUE

	while(TRUE) {
		if(ask) {
			which.plots <- menu(tmenu, title = 
				"\nMake plot selections (or 0 to exit):\n")
			if(any(which.plots == 1))
				which.plots <- 2:8
			which.plots <- 1 + which.plots
		}

		graph.number <- 1
		if(dev.cur() == 1 && which.plots[1] != 1)
			trellis.device()
		for(iwhich in 1:length(which.plots)) {
			pick <- which.plots[iwhich]
			switch(pick,
				return(invisible(x)),
				{
					ask.now <- FALSE
				}
				,
				{
          lmfmResVsFittedPlot(x,
            type = "deviance",
            ylab = "Deviance Residuals",
            main = "Deviance Residuals vs. Fitted Values",
            ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Deviances vs. Fitted Values")

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
          lmfmResQQPlot(x,
            type = "pearson",
            main = "Normal QQ Plot of Pearson Residuals",
            ylab = "Ordered Pearson Residuals",
            ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "QQ-Plot of Pearson Residuals")

						graph.number <- graph.number + 1
					}
				}
				,
				{
					fam <- family(x[[1]])$family
          n <- length(x[[1]]$fitted.values)
          r <- sapply(x, residuals, type = "deviance")
          f <- sapply(x, fitted)
          mod.names <- names(x)
          id.n <- 3
					if(fam == "Binomial")
						dist <- 0
					else dist <- 1
					qq.a <- y <- matrix(0, n, n.models)
					for(i in 1:n.models) {
						y[,i] <- x[[i]]$y
						qq.a[order(r[, i]), i] <- qqplot.glmRob(y[,i], f[,i], dist)$quantiles
					}

					df <- data.frame(	qq.a = as.vector(qq.a),
														qq.b = as.vector(r),
														mod = rep(mod.names, rep(n, n.models)))

					panel.special <- function(x, y, id.n = id.n) {
						panel.xyplot(x, y, col = 6, pch = 16)
						panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)
						invisible()
					}

					print(xyplot(qq.a ~ qq.b | mod,
						data = df,
						ylab = "Deviances",
						xlab = "Estimated Quantiles",
						main = "QQ Plot of Deviance Residuals",
						panel = panel.special,
						id.n = id.n,
						strip = function(...)
							strip.default(..., style = 1),
						...))

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Deviances QQ-Plot")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmSRvsRDPlot(x,
            type = "deviance",
            main = "Standardized Deviance Residuals vs. Robust Distances",
            xlab = "Robust Distances",
            ylab = "Deviance Residuals",
            ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Deviance Residuals vs. Robust Distances")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmStdResPlot(x,
            type = "deviance",
            main = "Standardized Deviance Residuals vs. Index (Time)",
            xlab = "Index (Time)",
            ylab = "Deviance Residuals",
            ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Deviances vs. Index")

						graph.number <- graph.number + 1
					}
				}
				,
				{
          lmfmSqrtResVsFittedPlot(x,
            type = "deviance",
            main = "Sqrt(abs(Deviance Residuals)) vs. Fitted Values",
            ylab = "Sqrt(abs(Deviance Residuals))",
            ...)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "sqrt(abs(Deviances)) vs. Fitted Values")

						graph.number <- graph.number + 1
					}
				}
			)
		}
	}
	invisible(x)
}


