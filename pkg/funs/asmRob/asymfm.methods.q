print.asymfm <- function(x, ...)
{
	digits <- options()$digits - 3
	fun.call <- match.call()
	n.models <- length(x)
	model.list <- attr(x, "model.list")
	mod.names <- format(names(model.list))

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	mod.coefs <- lapply(x, coef)
	coef.names <- unique(unlist(lapply(mod.coefs, names)))
	n.coefs <- length(coef.names)
	tmp <- matrix(NA, n.coefs, n.models)
	dimnames(tmp) <- list(coef.names, mod.names)
	for(i in 1:n.models)
		tmp[match(names(mod.coefs[[i]]), coef.names), i] <- mod.coefs[[i]]
	cat("\nCoefficients:\n")
	tmp.idx <- is.na(tmp)
	tmp <- format(round(tmp, digits = digits))
	tmp[tmp.idx] <- ""
	print(tmp, quote = F, ...)
	cat("\n")

	invisible(x)
}


summary.asymfm <- function(object, ...)
{
	n.models <- length(object)
	model.list <- attr(object, "model.list")

	ans <- list()
	ans$mod.names <- mod.names <- format(names(model.list))
	ans$calls <- lapply(object, function(x) x$call)

	mod.coefs <- lapply(object, coef)
	coef.names <- unique(unlist(lapply(mod.coefs, names)))
	n.coefs <- length(coef.names)
	ans$coefs <- array(NA, dim = c(n.coefs, 2, n.models))
	for(i in 1:n.models) {
		tmp <- summary(object[[i]])
		coef.idx <- match(names(mod.coefs[[i]]), coef.names)
		ans$coefs[coef.idx,  , i] <- tmp$coefficients
	}

	dimnames(ans$coefs) <- list(coef.names,
		dimnames(tmp$coefficients)[[2]], NULL)

	oldClass(ans) <- "summary.asymfm"
	ans
}


print.summary.asymfm <- function(x, ...)
{
	n.models <- length(x$calls)
	digits <- options()$digits - 3

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(x$mod.names[i], ": ")
		print(x$calls[[i]])
	}

	cat("\nCoefficients:\n")
	np <- dim(x$coefs)[1]
	tmp <- numeric()
	tmp.names <- character()

	if(np == 1) {
		for(j in 1:n.models) {
			tmp <- rbind(tmp, x$coefs[,  , j])
			tmp.names <- c(tmp.names, dimnames(x$coefs[,  , j, drop = F])[[1]])
		}
	}

	else {
		for(i in 1:np) {
			for(j in 1:n.models) {
			  tmp <- rbind(tmp, (x$coefs[,  , j])[i,  ])
			  tmp.names <- c(tmp.names, dimnames(x$coefs[,  , j])[[1]][i])
			}
		}
	}

	tmp.names <- paste(rep(paste(x$mod.names, ":"), np), format(tmp.names))
	dimnames(tmp)[[1]] <- tmp.names
	tmp.idx <- is.na(tmp)
	tmp <- format(round(tmp, digits = digits))
	tmp[tmp.idx] <- ""
	print(tmp, quote = F, ...)

	invisible(x)
}


plot.asymfm <- function(x, truncate = T, which.plots = "ask",
	lower.q = .999, plot.data = T, robustQQline = T, ...)
{
	model.list <- attr(x, "model.list")
	n.models <- length(x)
	xsum <- summary.asymfm(x)
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

	if(!is.null(x[[1]]$data))
		y <- x[[1]]$data
	else if(exists(x[[1]]$call$data))
		y <- eval(x[[1]]$call$data, sys.parent())
	else y <- NULL

	model.calls <- unlist(lapply(x, function(u) u$call[[1]]))

	##
	##  menu choices
	##

	choices <- c(	"All",
								"Overlaid Density Estimates", 
								"Response vs Estimated Quantiles")
	tmenu <- paste("plot:", choices)
	ask <- T

	if(!is.character(which.plots) && is.integer(which.plots)) {
		ask <- F	
		which.plots <- c(which.plots + 1, 1)
	}

	else if(which.plots == "all") {
		ask <- F
		which.plots <- c(3:4, 1)
	}

	while(T) {
		if(ask) {
			which.plots <- lmRob.checkbox(tmenu, title = 
				"\nMake plot selections (or 0 to exit):\n")
			if(any(which.plots == 1))
				which.plots <- 2:9
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
					##
					## Determine Model Types
					##

					tmp.main <- "Overlaid Densities"

					##
					## find the value of the largest quantile among models
					##

					x.lim <- rep(0, n.models)
					for(i in 1:n.models) {
						if(is.element("scale", arg.names(x[[i]]$quantile.fn)))
							x.lim[i] <- x[[i]]$quantile.fn(lower.q,
														shape = x[[i]]$alpha,
														scale = x[[i]]$sigma)
						else
							x.lim[i] <- x[[i]]$quantile.fn(lower.q,
														x[[i]]$alpha, x[[i]]$sigma)
					}

					x.lim <- max(x.lim)
					x.lim <- c(sqrt(.Machine$single.eps), x.lim)

					##
					## generate estimated densities
					##

					x.dens <- seq(x.lim[1], x.lim[2], length = 1000)
					y.dens <- matrix(0, ncol = n.models, nrow = 1000)

					for(i in 1:n.models) {
						if(is.element("scale", arg.names(x[[i]]$density.fn)))
							 y.dens[,i] <- x[[i]]$density.fn(x.dens,
																shape = x[[i]]$alpha,
																scale = x[[i]]$sigma)
						else
							y.dens[,i] <- x[[i]]$density.fn(x.dens,
																x[[i]]$alpha, x[[i]]$sigma)
					}

					y.lim <- c(0, max(y.dens))

					if(dev.cur() == 1)
						trellis.device()

					if(length(y) && plot.data) {
						data <- y[y < x.lim[2]]
						temp <- hist(data, nclass = "fd", probability = T, main = tmp.main,
							 xlab = "", plot = F)
						y.lim[2] <- max(y.lim[2], temp$counts)
						hist(data, nclass = "fd", probability = T, main = tmp.main,
							ylim = y.lim, xlab = "", plot = T, col = 9)
						x.range <- x.dens < max(temp$breaks)
						y.dens <- y.dens[x.range, , drop = F]
						x.dens <- x.dens[x.range]
					}

					else
						plot(0, 0, type = "n", xlim = x.lim, ylim = y.lim,
							main = tmp.main, xlab = "")

					for(i in 1:n.models)
						lines(x.dens, y.dens[, i], lty = styles[i], col = colors[i], lwd = 2)

					key(text = list(mod.names), lines = 1:n.models, col = colors,
						transparent = T, lty = styles, lwd = 2)

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Overlaid Densities")

						graph.number <- graph.number + 1
					}
				}
				,
				{
					if(!is.null(y)) {
						y <- sort(y)
						est.quantiles <- matrix(0, length(y), n.models)
						qt <- (1:length(y)) / (length(y) + 1)

						for(i in 1:n.models) {
							if(is.element("scale", arg.names(x[[i]]$quantile.fn)))
								est.quantiles[,i] <- x[[i]]$quantile.fn(qt,
																				shape = x[[i]]$alpha,
																				scale = x[[i]]$sigma)
							else
								est.quantiles[,i] <- x[[i]]$quantile.fn(qt,
																				x[[i]]$alpha, x[[i]]$sigma)
						}

						df <- data.frame(	x = as.vector(est.quantiles),
															y = rep(y, n.models),
															mod = rep(unlist(mod.names), each = length(y)))

						panel.special <- function(x, y, robline, ...) {
							panel.xyplot(x, y, pch = 16, col = 6, ...)
							panel.abline(coef(lmRob(y ~ x, mxf = 100)))
						}

						print(xyplot(	y ~ x | mod,
													data = df,
													panel = panel.special,
													robline = robustQQline,
													xlab = "Estimated Quantiles",
													ylab = "Response",
													layout = c(n.models, 1),
													strip = function(...) strip.default(..., style = 1)))
					}

					else warning(paste("Object \"", deparse(substitute(x)),
											"\" does not contain the original data.", sep = ""))

					if(names(dev.cur()) == "graphsheet") {
						guiModify("GraphSheetPage",
							Name = paste("$", graph.number, sep = ""),
							NewName = "Response vs. Estimated Quantiles")

						graph.number <- graph.number + 1
					}
				}
			)
		}
	}
	invisible(x)
}

