print.pcompfm <- function(x, ...)
{
	n.models <- length(x)
	model.list <- attr(x, "model.list")
	digits <- options()$digits - 3
	mod.names <- format(names(model.list))

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	stdev <- x[[1]]$sdev
	col.names <- names(stdev)
	if(n.models > 1)
		for (i in 2:n.models)
			stdev <- rbind(stdev, x[[i]]$sdev)
	stdev <- matrix(stdev, nrow = n.models, byrow = T)
	dimnames(stdev) <- list(mod.names, col.names)

	cat("\nStandard deviations:\n")
	print(stdev)

	cat("\nThe number of variables is ", dim(x[[1]]$loadings)[1],
	" and the number of observations is ", if(length(x[[1]]$n.obs))
	x[[1]]$n.obs else "unknown", ".\n", sep = "")

	invisible(x)
}


summary.pcompfm <- function(object, loadings = T, cutoff = 0.1, ...)
{
	fun.call <- match.call()
	n.models <- length(object)
	model.list <- attr(object, "model.list")

	ans <- list()
	ans$mod.names <- mod.names <- format(names(model.list))
	ans$calls <- lapply(object, function(x) x$call)

	columns <- length(object[[1]]$sdev)

	ans$sdev <- ans$pvar <- ans$qvar <-
		matrix(0, nrow = n.models, ncol = columns)

	dimnames(ans$sdev) <- dimnames(ans$pvar) <- dimnames(ans$qvar) <-
		list(mod.names, paste("Comp.",1:columns))

	for(i in 1:n.models) {
		ans$sdev[i,  ] <- object[[i]]$sdev
		variances <- object[[i]]$sdev^2
		ans$pvar[i,  ] <- variances/sum(variances)
		ans$qvar[i,  ] <- cumsum(ans$pvar[i,  ])
	}

	if(loadings) {
		if(is.integer(loadings)) {
			loadings <- min(loadings, sapply(object, function(x)
										{dim(x$loadings)[2]}))
			ans$loadings <- list()
			for(i in 1:n.models)
				ans$loadings[[i]] <- loadings(object[[i]])[, 1:loadings, drop = F]
		}
		else
			ans$loadings <- lapply(object, loadings)
		
		names(ans$loadings) <- mod.names
	}
	
	ans$cutoff <- cutoff
	oldClass(ans) <- "summary.pcompfm"
	ans
}


print.summary.pcompfm <- function(x, ...)
{
	digits <- options()$digits - 3

	n.models <- length(x$calls)
	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(x$mod.names[i], ": ")
		print(x$calls[[i]])
	}

	cat("\nImportance of components:\n\nStandard deviation\n")
	print(round(x$sdev, digits = digits))

	cat("\nProportion of Variance\n")
	print(round(x$pvar, digits = digits))

	cat("\nCumulative Proportion\n")
	print(round(x$qvar, digits = digits))

	if(length(x$loadings)) {
		if(is.numeric(x$cutoff))
		  cutoff <- x$cutoff
		else
		  cutoff <- 0.1
		cat("\nLoadings:")
		for (i in 1:n.models) {
		  cat("\n", format(x$mod.names[i]), "\n")
		  print.loadings(x$loadings[[i]], cutoff = cutoff)
		}
	}

	invisible(x)
}


plot.pcompfm <- function(x, which.plots = "ask", spcomps = "Auto", ...)
{
	model.list <- attr(x, "model.list")
	n.models <- length(x)
	xsum <- summary.pcompfm(x)
	if(!length(xsum$mod.names))
		xsum$mod.names <- paste("Model", 1:n.models)
	mod.names <- xsum$mod.names

	##
	##	Define colors, line styles and bandwidth fnct.
	##

	if(n.models < 5)
		colors <- c(1,6,4,8)[1:n.models]
	else colors <- 1:n.models

	if(n.models < 5)
		styles <- c(1,4,6,8)[1:n.models]
	else styles <- 1:n.models

	n.comps <- length(x[[1]]$sdev)

	##
	##  menu choices
	##

	choices <- c(	"All",
								"Trellis of Component Scatter Plots", 
								"Loadings",
								"Variances")
	tmenu <- paste("plot:", choices)

	ask <- T

	if(is.integer(which.plots)) {
		ask <- F
		which.plots <- c(which.plots + 1, 1)
	}

	else if(which.plots == "all") {
		ask <- F
		which.plots <- c(3:5, 1)
	}

	while(T) {
		if(ask) {
			which.plots <- lmRob.checkbox(tmenu, title = 
				"\nMake plot selections (or 0 to exit):\n")
			if(any(which.plots == 1))
				which.plots <- 2:4
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
				par(mfrow = c(1, 1))
				n.obs <- length(x[[1]]$scores) / n.comps
				fact <- comp <- NULL

				for(i in 1:n.models) {
					comp <- rbind(comp, x[[i]]$scores)
					fact <- c(fact, rep(mod.names[i], n.obs))
				}

				fact <- as.factor(fact)
				if(is.character(spcomps)) {
					if(spcomps == "Auto")
						spcomps <- 1:min(5, n.comps)
					else if(spcomps == "All")
						spcomps <- 1:n.comps
					}
				print(splom( ~ comp[, spcomps] | fact,
					pch = 16,
					strip = function(...)
						strip.default(..., style = 1),
					col = 6))
				title("Scatter Plot of Components", cex = 1.5)

				if(names(dev.cur()) == "graphsheet") {
					guiModify("GraphSheetPage",
						Name = paste("$", graph.number, sep = ""),
						NewName = "Components")

					graph.number <- graph.number + 1
				}
			}
			,
			{
				pages <- ceiling(n.comps/4)
				for(k in 1:pages) {
					par(mfrow = c(4, n.models))
					if(4 * k <= n.comps)
						this.page <- 4
					else
						this.page <- (n.comps - 4 * (k - 1))
					for(j in 1:this.page) {
						for(i in 1:n.models) {
							barplot(x[[i]]$loadings[ ,j],
								axes = F,
								space = 1,
								col = 6,
								main = paste(mod.names[i],
									": Comp.", 4 * (k - 1) + j, sep = ""))
							axis(2)
							axis(1, at = 2 * (1:length(x[[1]]$loadings[ ,1])),
								labels = dimnames(x[[1]]$loadings)[1])

						}
					}

					par(oma = c(0, 0, 2, 0))
					mtext(paste("Loadings - Page", k),
						side = 3, outer = T, cex = 1)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = paste("Loadings", k))

						graph.number <- graph.number + 1
					}

					if(exists("unix.time"))
						par(ask = T)
				}
				if(exists("unix.time"))
					par(ask = F)
			}
			,
			{
				plot.symbols <- 16:(15 + n.models)

				par(mfrow = c(1, 1))
				variances <- NULL
				for(i in 1:n.models)
					variances <- cbind(variances, x[[i]]$sdev^2)

				matplot(1:n.comps,
					variances,
					type = "o",
					axes = F,
					ylab = "Variances",
					lty = 4 * ((1:n.models) - 1) + 1,
					pch = plot.symbols,
					col = colors,
					main = "Variances")

				axis(2)
				axis(1, at = 1:n.comps,
					labels = names(x[[1]]$sdev))
				key(text = list(mod.names),
					transparent = T,
					lines = list(	type = "o",
												lty = 4 * ((1:n.models) - 1) + 1,
												pch = plot.symbols,
												col = colors))

				if(names(dev.cur()) == "graphsheet") {
					guiModify("GraphSheetPage",
						Name = paste("$", graph.number, sep = ""),
						NewName = "Screeplot")

					graph.number <- graph.number + 1
				}
			}
			)
		}
	}

	invisible(x)
}
