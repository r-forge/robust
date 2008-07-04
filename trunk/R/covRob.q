covRob <- function(data, corr = FALSE, distance = TRUE, na.action = na.fail,
                   estim = "auto", control = covRob.control(estim, ...), ...)
{
  ## Step 1. Check the data and control options ##

	data <- na.action(data)
  if(is.data.frame(data))
    data <- data.matrix(data)

	n <- nrow(data)
	p <- ncol(data)

	rowNames <- dimnames(data)[[1]]
	colNames <- dimnames(data)[[2]]
	dimnames(data) <- NULL

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

  ## For backwards compatibility we support the use of quan and ntrial   ##
  ## to specify alpha and nsamp for estim = "mcd", estim = "weighted"    ##
  ## and estim = "M". Providing both quan and alpha or both ntrial and   ##
  ## nsamp will result in an error.                                      ##

    if(any(dots.names == "quan") && all(dots.names != "alpha")) {
      dots.names[dots.names == "quan"] <- "alpha"
      names(dots) <- dots.names
    }

    if(any(dots.names == "ntrial") && all(dots.names != "nsamp")) {
      dots.names[dots.names == "ntrial"] <- "nsamp"
      names(dots) <- dots.names
    }

		control.names <- names(control)
    if(any(control.names == "init.control"))
      control.names <- c(control.names, names(control$init.control))
		if(any(!is.element(dots.names, control.names))) {
			bad.args <- sQuote(setdiff(dots.names, control.names))
			if(length(bad.args) == 1)
				stop(sQuote(bad.args), " is not a control argument for the ",
             dQuote(estim), " estimator")
			else
				stop(paste(sQuote(bad.args), collapse = ", "), " are not control ",
             "arguments for the ", dQuote(estim), " estimator")
		}
	}

  ## Step 2. Call the robust covariance estimator ##

	ans <- switch(estim,

		donostah = donostah(data, control),

		pairwiseqc = fastcov(data, control),

		pairwisegk = fastcov(data, control),

		m = {
      mcd.control <- control$init.control
      control$init.control <- NULL
			if(mcd.control$alpha > 1)
				mcd.control$alpha <- mcd.control$alpha / n

      init <- covMcd(data, cor = FALSE, control = mcd.control)

			ans <- covMest(data, cor = FALSE, r = control$r, arp = control$arp,
                     eps = control$eps, maxiter = control$maxiter,
                     t0 = init$raw.center, S0 = init$raw.cov)

      ans$dist <- ans$mah
      ans$raw.center <- init$raw.center
      ans$raw.cov <- init$raw.cov
      ans$raw.dist <- init$raw.mah
      ans
		},

		mcd = {
			if(control$alpha > 1)
				control$alpha <- control$alpha / n

      ans <- covMcd(data, cor = FALSE, control = control)

      ans$center <- ans$raw.center
      ans$cov <- ans$raw.cov
      ans$dist <- ans$raw.mah
      ans$raw.cov <- ans$raw.cov / prod(ans$raw.cnp2)
      ans$raw.dist <- ans$raw.mah * prod(ans$raw.cnp2)
			ans
		},

		weighted = {
			if(control$alpha > 1)
				control$alpha <- control$alpha / n

      ans <- covMcd(data, cor = FALSE, control = control)

      ans$dist <- ans$mah
      ans$raw.cov <- ans$raw.cov / prod(ans$raw.cnp2)
      ans$raw.dist <- ans$raw.mah * prod(ans$raw.cnp2)

			ans
		},

		default = stop("Invalid choice of estimator.")

	) # end of switch


  ## Step 3. Create output object ##

	dimnames(ans$cov) <- list(colNames, colNames)
	names(ans$center) <- colNames

  ## If no initial estimate set as missing ##

  if(is.null(ans$raw.cov)) {
    ans$raw.cov <- NA
    ans$raw.center <- NA
  }

	else {
		dimnames(ans$raw.cov) <- list(colNames, colNames)
		names(ans$raw.center) <- colNames
	}

	## compute Mahalanobis distances ##

	if(distance) {
    if(is.null(ans$dist))
      ans$dist <- mahalanobis(data, ans$center, ans$cov)
    if(!is.na(ans$raw.cov[1])) {
      if(is.null(ans$raw.dist))
        ans$raw.dist <- mahalanobis(data, ans$raw.center, ans$raw.cov)
    }
    else
      ans$raw.dist <- NA
  }
  
  else {
    ans$dist <- NA
    ans$raw.dist <- NA
  }

	if(!is.na(ans$dist[1]) && !is.null(rowNames))
		names(ans$dist) <- rowNames
	if(!is.na(ans$raw.dist[1]) && !is.null(rowNames))
		names(ans$raw.dist) <- rowNames

	## covariance or correlation ##

	if(corr) {
		std <- sqrt(diag(ans$cov))
		ans$cov <- ans$cov / (std %o% std)

		if(!is.na(ans$raw.cov[1])) {
			std <- sqrt(diag(ans$raw.cov))
			ans$raw.cov <- ans$raw.cov / (std %o% std)
		}
	}

	ans$corr <- corr

	## some diagnostic information ##

	ans$estim <- estim
	ans$control <- control
	ans$call <- match.call()

	ans <- ans[c("call", "cov", "center", "dist", "raw.cov", "raw.center",
               "raw.dist", "corr", "estim", "control")]
	oldClass(ans) <- "covRob"
	ans
}

