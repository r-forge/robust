fastcov <- function(x, control)
{
	the.call <- match.call()

	n <- nrow(x)
	p <- ncol(x)

	iter <- 2
	c1 <- 4.5
	c2 <- 2.36075
	beta <- 0.9
	covmat <- matrix(0, p, p)
	loc <- rep(0, p)
	dwork <- rep(0, n)
	dmedian <- 0
	estim <- control$estim

	if(estim == "pairwiseqc")
		pairwise.estimator <- 0
	else
		pairwise.estimator <- 1

	storage.mode(covmat) <- "double"

	fclist <- .C("rl_fastcov",
								x = as.double(x),
								n = as.integer(n),
								p = as.integer(p),
								iter = as.integer(iter),
								c1 = as.double(c1),
								c2 = as.double(c2),
								raw.cov = covmat,
								raw.center = as.double(loc),
								d = as.double(dwork),
								dmedian = as.double(dmedian),
								pairwise.estimator = as.integer(pairwise.estimator),
                PACKAGE = "robust")

## check against original code ##

	#fclist$covmat <- fclist$covmat / qchisq(.5, p)
	c3 <- qchisq(beta, p)	
	c4 <- c3 * fclist$dmedian / qchisq(.5, p)

	r <- fclist$d < c4
	rr <- mean(r)
	V1 <- cov.wt(x[r,])
	V1$cov <- rr * V1$cov / pchisq(c3, p+2)

	fclist$cov <- V1$cov
	fclist$center <- V1$center
	fclist$call <- the.call

	fclist[c("call", "cov", "center", "raw.cov", "raw.center")]
}

