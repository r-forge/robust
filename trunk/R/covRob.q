covRob <- function(data, corr = FALSE, distance = TRUE,
				na.action = na.fail, estim = "auto",
				control = covRob.control(estim, ...), ...)
{
	stop.on.bdObject(data)
	the.call <- match.call()

##
## Step 1. Check the data and control options
##

	data <- na.action(data)
	data <- as.matrix(data)

	n <- nrow(data)
	p <- ncol(data)

	rowNames <- dimnames(data)[[1]]
	colNames <- dimnames(data)[[2]]
	dimnames(data) <- NULL

	if(is.null(rowNames))
		rowNames <- 1:n

	if(is.null(colNames))
		colNames <- paste("V", 1:p, sep = "")

	if(p < 2)
	    stop("Need at least two columns in data to compute a covariance")
	if(n < p)
	    stop("Not enough data")

	estim <- casefold(estim)

	if(estim == "auto") {
		if((n < 1000 && p < 10) || (n < 5000 && p < 5))
			estim <- "donostah"
		else if(n < 50000 && p < 20)
			estim <- "mcd"
		else
			estim <- "pairwiseqc"
		control <-  covRob.control(estim)
	}

	else {
		dots <- list(...)
		dots.names <- names(dots)
		control.names <- names(control)
		if(any(!is.element(dots.names, control.names))) {
			bad.args <- sQuote(setdiff(dots.names, control.names))
			estim <- dQuote(estim)
			if(length(bad.args) == 1)
				stop(paste(bad.args, "is not a control argument for the", estim, "estimator"))
			else
				stop(paste(paste(bad.args, collapse = ", "), "are not a control arguments for the", estim, "estimator"))
		}
	}

##
## Step 2. Call the robust covariance estimator
##

	ans <- switch(estim,

		donostah = {
			donostah(data, control)
		},

		pairwiseqc = {
			fastcov(data, control)
		},

		pairwisegk = {
			fastcov(data, control)
		},

		m = {
			rockem(data, control)
		},

		mcd = {

			quan <- control$quan
			ntrial <- control$ntrial

			if(is.numeric(quan) && 0.5 < quan && quan < 1)
				quan <- ceiling(quan * n)

      if(quan < floor((n + p + 1)/2) || quan > n)
        stop("The value for quan must be between (n+p+1)/2 and n")

			ans <- fastmcd(data, print = FALSE, quan = quan, ntrial = ntrial)
			ans <- ans[c("call", "cov", "center", "raw.cov", "raw.center")]
			ans$cov <- ans$raw.cov
			ans$center <- ans$raw.center
			ans
		},

		weighted = {

			quan <- control$quan
			ntrial <- control$ntrial

			if(is.numeric(quan) && 0.5 < quan && quan < 1)
				quan <- ceiling(quan * n)

      if(quan < floor((n + p + 1)/2) || quan > n)
        stop("The value for quan must be between (n+p+1)/2 and n")

			ans <- fastmcd(data, print = FALSE, quan = quan, ntrial = ntrial)
			ans <- ans[c("call", "cov", "center", "raw.cov", "raw.center")]
			ans
		},

		default = stop("Invalid choice of estimator.")

	) # end of switch


##
## Step 3. Create output object
##

	## put names on output ##

	dimnames(ans$cov) <- list(colNames, colNames)
	names(ans$center) <- colNames

	if(!is.null(ans$raw.cov)) {
		dimnames(ans$raw.cov) <- list(colNames, colNames)
		names(ans$raw.center) <- colNames
	}

  ## If no initial estimate set as missing ##

  if(is.null(ans$raw.cov)) {
    ans$raw.cov <- NA
    ans$raw.center <- NA
  }

	## compute Mahalanobis distances ##

	if(distance && is.null(ans$dist))
		ans$dist <- mahalanobis(data, ans$center, ans$cov)

	if(!is.null(ans$dist))
		names(ans$dist) <- rowNames

	## covariance or correlation ##

	if(corr) {
		std <- sqrt(diag(ans$cov))
		ans$cov <- ans$cov / (std %o% std)

		if(!is.null(ans$raw.cov)) {
			std <- sqrt(diag(ans$raw.cov))
			ans$raw.cov <- ans$raw.cov / (std %o% std)
		}
	}

	ans$corr <- corr

	## some diagnostic information ##

	ans$estim <- estim
	ans$control <- control
	ans$call <- the.call

	ans <- ans[c("call", "cov", "center", "raw.cov", "raw.center", "dist", "corr", "estim", "control")]
	oldClass(ans) <- "covRob"
	ans
}

