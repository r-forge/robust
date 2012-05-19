########################################################################
#
#			Robust Principle Components
#
########################################################################

princompRob <- function(x, data = NULL, covlist = NULL,
				scores = T, corr = F, na.action, subset, estim = "auto",
				control = covRob.control(estim, ...), ...)

{
	cmat <- center <- Terms <- scales <- NULL
	call <- match.call()

#	call.control.args <- is.element(names(call),names(control))
#	control.args.in.call <- is.element(names(control), names(call))
#	if(sum(control.args.in.call) > 0)
#	{
#		control[sort(names(control)[control.args.in.call])] <-
#			lapply(as.list(call)[sort(names(call)[call.control.args])], eval)
#	}

	if(is.list(covlist)) {
		center <- covlist$center
		cmat <- covlist$cov
		if(is.null(cmat) || is.null(center))
        stop("covlist missing either center or cov component")
		p <- length(center)
		d <- dim(cmat)
		if(p != d[1] || p != d[2])
			stop("bad dimension in covlist$cov or covlist$center")
		n.obs <- covlist$n.obs
		if(!length(cennam <- names(center)))
			cennam <- paste("X", 1:p, sep = "")
		if(!length(dimnames(cmat)[[1]]))
			dimnames(cmat) <- list(cennam, cennam)
	}

  if(!missing(x)) {
		if(form.p <- any(inherits(x, c("formula", "terms")))) {
			m <- call
			if(is.matrix(eval(m$data, sys.parent())))
				m$data <- as.data.frame(data)
				m$covlist <- m$corr <- m$scores <- NULL
				m[[1]] <- as.name("model.frame")
				names(m)[names(m) == "x"] <- "formula"
				m <- eval(m, sys.parent())
				Terms <- attr(m, "terms")
				if(attr(Terms, "response"))
					stop("a response in the formula is inappropriate")
				attr(Terms, "intercept") <- 0
				x <- model.matrix(Terms, m)  
		}

		else x <- as.matrix(x)
	}

	else {
		if(length(data))
			x <- as.matrix(data)
		else x <- NULL
	}

	if(length(x)) {
		if(mode(x) != "numeric")
			stop("not all the data are numeric")

		if(!length(covlist)) {
			if(!missing(subset) && !form.p)
				x <- x[subset,  , drop = F]  
			if(missing(na.action))
				na.fail(x)
			else x <- na.action(x)
			d <- dim(x)
			if(is.null(d))
				stop("no useful data")
			n.obs <- d[1]
			p <- d[2]
			if(!length(dimnames(x)[[2]]))
				dimnames(x) <- list(dimnames(x)[[1]], paste("X", 1:p, sep = ""))
			covlist <- covRob(x, center = T, corr = F, distance = F,
												estim = estim, control = control)
			cmat <- covlist$cov
			center <- covlist$center
		}

		else if(p != dim(x)[2] && scores)
			stop("data and covlist do not match")
	}

	else if(!length(covlist))
		stop("no data given")   

	if(corr) {
		scales <- sqrt(diag(cmat))
		cmat <- cmat/scales/rep(scales, rep.int(p, p))
	}

	else scales <- rep(1, p)
	ans <- eigen(cmat, symmetric = T) 
	values <- ans$values
	values[values < 0] <- 0
	ans$values <- values
	corrs <- diag(cmat)^-0.5 * ans$vectors * rep(values^0.5, rep.int(p,p))
	names(ans) <- c("sdev", "loadings")[match(c("values", "vectors"), names(ans))]
	ans$sdev <- sqrt(ans$sdev)
	cnames <- paste("Comp.", 1:length(ans$sdev))
	names(ans$sdev) <- cnames
	dimnames(corrs) <- dimnames(ans$loadings) <- list(dimnames(cmat)[[1]], cnames)
	ans$correlations <- corrs

	if(scores && length(x)) {
		if(corr)
			x <- scale(x, center = center, scale = scales)
		else x <- x - rep(center, rep.int(dim(x)[1], p))

		if(is.logical(scores))  
			scores <- p
		else ans$loadings <- ans$loadings[, 1:scores, drop = F]

		if(length(cmat)) {
			mnames <- match(dimnames(x)[[2]], dimnames(ans$loadings)[[1]], nomatch = 0)
			if(length(mnames) && any(mnames != 1:p))
				stop("mismatch between x and covlist")
		}

		ans$scores <- x %*% ans$loadings
	}

	else if(is.numeric(scores) && scores)
		ans$loadings <- ans$loadings[, 1:scores, drop = F]

	oldClass(ans$loadings) <- "loadings"
	ans$center <- center
	ans$scale <- scales
	ans$n.obs <- n.obs
	ans$terms <- Terms
	ans$call <- call
	ans$factor.sdev <- ans$sdev
	ans$coef <- ans$loadings 
	oldClass(ans) <- "princompRob"
	ans
}



########################################################################

print.princompRob <- function(x, ...)
{
	cat("Standard Deviations:\n")
	print(x$sdev, ...)
	cat("\nThe number of variables is", dim(x$loadings)[1], "and the number of observations is", 
		if(length(x$n.obs)) x$n.obs else "unknown.", "\n")
	cat("\nComponent names:\n")
	namx <- names(x)
	names(namx) <- rep(" ", length(namx))
	print(namx, quote = T)
	cat("\nCall:\n")
	print(x$call, ...)
	invisible(x)
}


########################################################################

summary.princompRob <- function(x, loadings = F, cutoff = 0.1)

{
	sdev <- x$sdev
	vars <- sdev^2
	vars <- vars/sum(vars)
	if(loadings) {
		if(is.numeric(loadings)) {
			loadings <- min(loadings, dim(x$loadings)[2])
			lds <- x$loadings[, 1:loadings, drop = F]
		}

		else lds <- x$loadings
	}

	else lds <- NULL

	varmat <- rbind("Standard deviation" = sdev,
									"Proportion of Variance"  = vars,
									"Cumulative Proportion" = cumsum(vars))

	ans <- list(varmat = varmat, loadings = lds, cutoff = cutoff)
	oldClass(ans) <- "summary.princompRob"
	ans
}

########################################################################

print.summary.princompRob <- function(x, cutoff = x$cutoff, ...)
{
	cat("\nImportance of components:\n")
	print(x$varmat, ...)
	if(!is.null(x$loadings)) {
		cat("\nLoadings:\n")
		print.loadings(x$loadings, cutoff = cutoff, ...)
	}
	invisible(x)
}	

########################################################################

plot.princompRob <- function(x, which.plots = "ask", ...)

{
	x.name <- deparse(substitute(x))
	model.list <- list(x$call)
	names(model.list) <- x.name
	x <- list(x = x)
	names(x) <- x.name
	attr(x, "model.list") <- model.list

	plot.pcompfm(x, which.plots = which.plots, ...)

	invisible(x[[1]])
}

########################################################################

princomp.fix <- function(x, data = NULL, covlist = NULL,
							scores = T, corr = F, na.action, subset,
							control = covRob.control("auto"))
{
	fun.call <- match.call()
	fun.call[[1]] <- as.name("princomp")
	fun.args <- is.element(arg.names(fun.call), c("x", "data", "covlist",
																	"scores", "corr", "na.action", "subset"))
	fun.call <- fun.call[c(T, fun.args)]
	corr.arg <- match("corr", arg.names(fun.call), nomatch = F)
	if(corr.arg) names(fun.call)[[1+corr.arg]] <- "cor"
	eval(fun.call)
}







