image.cov <- function(object, probs = c(.95, .99), ...)
{
	correlation.matrix <- function(x) {
		if(x$corr) return(x$cov)
		s <- sqrt(diag(x$cov))
		x$cov / (s %o% s)
	}

	if(length(object) != 2)
		stop("Comparison requires exactly two models.")

	if(platform() != "WIN386")
		stop("\timage.cov requires the graphsheet device that is\navailable only on the Windows platform.\nProblem")

        n <- length(object[[1]]$dist)
        X <- lapply(object, correlation.matrix)
        X <- lapply(X, function(x) {x[row(x) <= col(x)] <- 0; x})
        X <- lapply(X, function(x) {x[row(x) <= col(x)] <- .5 *
                logb((1 + x[row(x) <= col(x)]) / (1 - x[row(x) <= col(x)])); x})
        X <- abs(X[[1]] - X[[2]]) * sqrt(n - 3)
        p <- dim(X)[1]
        X <- X[,p:1, drop = F]
        qprobs <- qnorm((1 + probs)/2)
        qprobs <- c(0, .Machine$double.eps, qprobs, Inf)
        X <- matrix(cut(X, qprobs, include.lowest = T), p, p)
        shades <- as.character(length(qprobs) - 3)

        if(dev.cur() != 1) {
            current.device <- dev.cur()
            on.exit(dev.set(which = current.device))
        }

        graphsheet(image.color.table = "255,255,255|0,0,0",
                num.image.colors = 2,
                num.image.shades = shades)

        par(pty = "s")
        image(X, zlim = c(1, length(qprobs) - 1), axes = F)
        box()
        axis(4, at = p + 1 - c(1, floor(p/5)*(1:5)),
            labels = c(1, floor(p/5)*(1:5)), adj = 0)
            axis(3, at = c(1, floor(p/5)*(1:5)),
            labels = c(1, floor(p/5)*(1:5)))

        plot.legend <- function(colors, zlim, probs) {
                        image(colors, axes = F, zlim = zlim)
			box()
			labels <- paste(format(100 * c(0, probs)), "%", sep = "")
			axis(2, at = 1:(length(probs) + 1), labels = labels, ticks = T, adj = 1)
                        }

        colors <- matrix(2:(length(qprobs) - 1), nrow = 1)
        usr <- par()$usr

        subplot(plot.legend(colors, c(1, length(qprobs) - 1), probs),
            x = usr[1] + .2 * (usr[2] - usr[1]), y = usr[3] + .3 * (usr[4] - usr[3]),
            size = c(.8, 3))

	invisible()
}

identify.cov <- function(object) {

	correlation.matrix <- function(x) {
		if(x$corr) return(x$cov)
		s <- sqrt(diag(x$cov))
		x$cov / (s %o% s)
	}

	p <- dim(object[[1]]$cov)[1]
	loc <- locator(1)
	y <- round(loc$x)
	x <- p + 1 - round(loc$y)
	if(x >= y) {
		cat("Message: No cell selected.\n")
		return(invisible())
	}
	the.names <- dimnames(object[[1]]$cov)[[1]]
	if(!length(the.names))
		the.names <- paste("V", 1:p, sep = "")
	models <- names(object)
	object <- lapply(object, correlation.matrix)
	corrs <- matrix(sapply(object, function(u, x, y) u[x,y], x = x, y = y), ncol = 1)
	dimnames(corrs) <- list(paste(models, ":", sep = ""), "correlation")
	cat(paste("Correlation between", the.names[x], "and", paste(the.names[y], ":", sep = ""), "\n")) 
	corrs
}


