donostah <- function(x, control)
{
	n <- nrow(x)
	p <- ncol(x)

  center <- control$center
	nresamp <- control$nresamp
	maxres <- control$maxres
	prob <- control$prob
	eps <- control$eps

	if(!control$random.sample) {
    if(exists(".Random.seed", where = 1)) {
      random.seed <- get(".Random.seed", pos = 1)
      on.exit(assign(".Random.seed", random.seed, pos = 1))
    }
		set.seed(21)
	}

  if(casefold(nresamp) == "auto")
    nresamp <- ceiling(log(1 - control$prob)/log(1 - (1 - control$eps)^(p+1)))

	else if(!is.integer(nresamp)) 
		stop("nresamp must be a nonnegative integer or ", dQuote("auto"))

	if(nresamp != 0)
		nresamp <- max(1000, nresamp)

	if(casefold(maxres) == "auto")
		maxres <- 2 * nresamp

	else if(!is.integer(maxres))
		stop(sQuote("maxres"), " is not a positive integer")

	tune <- sqrt(qchisq(control$tune, p))

	icent <- 1
	locat <- double(p)
	covmat <- matrix(0.0, p, p)
	storage.mode(covmat) <- "double"
	wk <- double(4*n+p)
	iwork <- integer(4*n+p)
	nresper <- 0
	w <- double(n)
	z <- double(n)

	if(length(center) == 1 && !center)
		center <- rep(0, p)

	if(length(center) > 1) {
		if(length(center) != p)
			stop("the dimension of ", sQuote("center"), " does not match the ",
           "dimension of ", sQuote("x"))
		x <- sweep(x, 2, center)
		icent <- 0
	}

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

  consistency.correction <- median(dist) / qchisq(.5, p)
	sdlist$cov <- sdlist$cov * consistency.correction
	sdlist$dist <- dist / consistency.correction

	if(length(center) > 1)
		sdlist$center <- center

	sdlist[c("cov", "center", "dist")]
}
