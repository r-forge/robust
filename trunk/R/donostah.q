donostah <- function(x, control)
{
	the.call <- match.call()

	n <- nrow(x)
	p <- ncol(x)

## extract and process the control parameters ##

  center <- control$center
	nresamp <- control$nresamp
	maxres <- control$maxres
	random.sample <- control$random.sample
	tune <- control$tune
	prob <- control$prob
	eps <- control$eps

	if(!random.sample) {
		old.random.seed <- .Random.seed
		on.exit(assign(".Random.seed", old.random.seed))
		set.seed(21)
	}

	if(casefold(nresamp) == "auto")
		nresamp <- ceiling(log(1-control$prob)/log(1 - (1 - control$eps)^(p + 1)))

	else if(!is.integer(nresamp)) 
		stop("nresamp must be 0, a positive integer, or \"auto\".")

	if(nresamp != 0)
		nresamp <- max(1000, nresamp)

	if(casefold(maxres) == "auto")
		maxres <- 2 * nresamp

	else if(!is.integer(maxres))
		stop("maxres must be a positive integer.")

	tune <- sqrt(qchisq(control$tune, p))

## allocate memory for the Fortran subroutine ##

	icent <- 1
	locat <- rep(0, p)
	covmat <- matrix(0, p, p)
	wk <- rep(0, 4*n+p)
	iwork <- rep(0, 4*n+p)
	nresper <- 0
	w <- rep(0, n)
	z <- rep(0, n)

	if(length(center) == 1 && !center)
		center <- rep(0, p)

	if(length(center) > 1) {
		if(length(center) != p)
			stop("Dimension of center and data do not match")
		x <- sweep(x, 2, center)
		icent <- 0
	}

## covmat needs to retain its matrixness ##

	storage.mode(covmat) <- "double"

	sdlist <- .Fortran("rlds",
                      n = as.integer(n),
                      p = as.integer(p),
                      nresamp = as.integer(nresamp),
                      x = as.double(x),
                      tune = as.double(tune),
                      wk = as.double(wk),
                      center = as.double(locat),
                      cov = covmat,
                      maxres = as.integer(maxres),
                      nresper = as.integer(nresper),
                      weights = as.double(w),
                      outlyingness = as.double(z),
                      icent = as.integer(icent),
                      iwork = as.integer(iwork),
                      PACKAGE = "robust")

	dist <- mahalanobis(x,
		center = if(length(center) > 1) rep(0, p) else sdlist$center,
		cov = sdlist$cov)

	sdlist$covmat <- sdlist$covmat * median(dist) / qchisq(.5, p)
	sdlist$dist <- dist

	if(length(center) > 1)
		sdlist$center <- center

	sdlist$call <- the.call
	sdlist[c("call", "cov", "center", "dist")]
}
