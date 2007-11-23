fastmcd <- function(x, cor = FALSE, print.it = TRUE, quan = floor((n+p+1)/2),
                            ntrial = 500)
{ 
  if(is.vector(x) || (is.matrix(x) && !is.data.frame(x))) {
    if(!is.numeric(x))
      stop("x is not a numeric dataframe or matrix.")
  }
  if((!is.vector(x) && !is.matrix(x)) || is.data.frame(x)) {
    if((!is.data.frame(x) && !is.numeric(x)) || 
       (!all(sapply(x, data.class) == "numeric")))
      stop("x is not a numeric dataframe or matrix.")
  }
  #if(!is.matrix(x))
  #  x <- array(x, c(length(x), 1), 
  #             list(names(x), deparse(substitute(data))))
  x <- as.matrix(x)
  dimn <- dimnames(x)
  na.x <- !is.finite(rowSums(x))
  ok <- !na.x
  x <- x[ok,  , drop = FALSE]
  dx <- dim(x)
  if(!length(dx))
    stop("All observations have missing values!")
  n <- dx[1]
  p <- dx[2]
  if(n < 2 * p)
    stop("Need at least 2*(number of variables) observations ")
  jmin <- floor((n + p + 1)/2)
# storage.mode(jmin) <- "integer"
# storage.mode(quan) <- "integer"
  if(quan < jmin) {
    stop("The MCD must cover at least", jmin, "observations")
  }
  else if(quan > n)
    stop("quan is greater than the number of non-missing values!")
  ans <- list()
  ans$call <- match.call()
  ans$method <- paste("Minimum Covariance Determinant Estimator.")
  ans$quan <- quan
  ans$mcd.wt <- rep(NA, length(na.x))
  if(length(dimn[[1]])) 
    names(ans$mcd.wt) <- dimn[[1]]
  ans$X <- x
  if(length(dimn[[1]]))
    dimnames(ans$X)[[1]] <- names(ans$mcd.wt)[ok]
  else {
    xx <- seq(1, length(na.x))
    dimnames(ans$X) <- list(NULL, NULL)
    dimnames(ans$X)[[1]] <- xx[ok]
  }
##
## Case 1: quan = n ==> raw.cov = var
##
  if(quan == n) {
    mcd <- as.matrix(var(x))
    loc <- as.vector(colMeans(x))
    eigs <- eigen(mcd, symm=TRUE, only=TRUE)$values
    ans$raw.objective <- prod(eigs)
    ans$raw.cov <- mcd
    dimnames(ans$raw.cov) <- list(dimn[[2]], dimn[[2]])
    ans$raw.center <- loc
    names(ans$raw.center) <- dimn[[2]]
##
## Case 1a: singular raw.cov
##    
    if(any(eigs < 0)) {
      ans$cov    <- ans$raw.cov
      ans$center <- ans$raw.center
      ans$n.obs <- n
      ans$method <- paste(ans$method, 
                          "\nThe classical covariance matrix is singular.")
      ans$mcd.wt[ok] <- rep(1, sum(ok == TRUE))
    }
    else {
##
## Case 1b: proceed to reweight
##  
      mah <- mahalanobis(x, loc, mcd)
      weights <- ifelse(mah < qchisq(0.975, p), 1, 0)
      tmp <- cov.wt(x, wt = weights, cor)
      ans$cov <- sum(weights)/(sum(weights) - 1) * tmp$cov
      ans$cov <- as.matrix(ans$cov)
      ans$center <- tmp$center
      ans$n.obs  <- tmp$n.obs
      ans$mcd.wt[ok] <- weights
      eigs <- eigen(ans$cov, symm=TRUE, only=TRUE)$values
      if(any(eigs < 0)) {
        ans$method <- paste(ans$method, 
                      "\nThe reweighted MCD scatter matrix is singular.") 
      }
      ans$method <- paste(ans$method, 
                    "\nThe minimum covariance determinant estimates based on",
                    n, "observations \nare equal to the classical estimates.")
    }
    if(print.it) {
      cat(ans$method, "\n")
    }
    oldClass(ans) <- "mcd"
    attr(ans, "call") <- sys.call()
    return(ans)
  }
##
## Case 2: quan < n ==> use fast MCD
##
  storage.mode(x) <- "double"
  storage.mode(quan) <- "integer"
  initcov <- matrix(0, nrow = p * p, ncol = 1)
  adcov <- matrix(0, nrow = p * p, ncol = 1)
  initmean <- matrix(0, nrow = p, ncol = 1)
  inbest <- matrix(10000, nrow = quan, ncol = 1)
  plane <- matrix(0, nrow = 5, ncol = p)
  weights <- matrix(0, nrow = n, ncol = 1)
  deter <- fit <- kount <- 0
  storage.mode(n) <- storage.mode(p) <- storage.mode(ntrial) <- "integer"
  storage.mode(initcov) <- storage.mode(adcov) <- 
                           storage.mode(initmean) <- "double"
  storage.mode(inbest) <- "integer"
  storage.mode(plane) <- storage.mode(deter) <- "double"
  storage.mode(weights) <- storage.mode(fit) <- 
                           storage.mode(kount) <- "integer"
  mcd <- .C("robust_fastmcd",
		x,
		n,
		p,
		quan,
		ntrial,
		initcovariance = initcov,
		initmean = initmean,
		inbest,
		mcdestimate = deter,
		weights = weights,
		exactfit = fit,
		coeff = plane,
		kount = kount,
		adjustcov = adcov,
		nmax = n,
    PACKAGE = "robust")

	dim(mcd$initcovariance) <- c(p, p)
	robust.distances <- mahalanobis(x, mcd$initmean, mcd$initcovariance)
	median.robust.distance <- median(robust.distances)
	mcd$initcovariance <- median.robust.distance * mcd$initcovariance / qchisq(.5, p)

##
## Case 2a: p = 1
##
  if(p == 1) {
    center <- as.double(mcd$initmean)
    scale <- as.double(mcd$initcov)
    ans$raw.cov <- scale^2
    names(ans$raw.cov) <- dimn[[2]][1]
    ans$raw.center <- as.vector(center)
    names(ans$raw.center) <- dimn[[2]][1]
    if(abs(scale) < 1e-10) {
      ans$raw.objective <- 0
      scale <- 0
      names(scale) <- dimn[[2]][1]
      ans$cov <- ans$center <- scale
      ans$n.obs <- n
      ans$method <- paste("Univariate location and scale estimation.\n",
                          "More than", quan, 
                          "of the observations are identical.")
      ans$mcd.wt[ok] <- rep(1, sum(ok == TRUE))
    }
    else {
      weights <- ifelse(abs((x - center)/scale) < 2.5, 1, 0)
      tmp <- cov.wt(x, wt = weights, cor = cor)
      ans$cov <- sum(weights)/(sum(weights) - 1) * tmp$cov
      ans$center <- tmp$center
      ans$n.obs  <- tmp$n.obs
      ans$method <- paste("Univariate location and scale estimation.")   
      ans$raw.objective <- (1/(quan -1))*sum(sort(
                           (x-as.double(mcd$initmean))^2, quan)[1:quan])
      ans$mcd.wt[ok] <- weights
    }
    if(print.it) {
      cat(ans$method, "\n")
    }
    oldClass(ans) <- "mcd"
    attr(ans, "call") <- sys.call()
    return(ans)
  }
##
## Case 2b: p > 1
##
  ans$raw.cov <- mcd$initcovariance
  dimnames(ans$raw.cov) <- list(dimn[[2]], dimn[[2]])
  ans$raw.center <- as.vector(mcd$initmean)
  names(ans$raw.center) <- dimn[[2]]
  ans$mcd.wt[ok] <- as.vector(mcd$weights)
  if(mcd$exactfit != 0) {
    dim(mcd$coeff) <- c(5, p)
    ans$raw.objective <- 0
    ans$cov    <- ans$raw.cov
    ans$center <- ans$raw.center
    ans$n.obs <- n
    if(mcd$exactfit == -1) {
      stop(paste("The program allows for at most ", mcd$kount, 
                 " observations."))
    }
    if(mcd$exactfit == -2) {
      stop(paste("The program allows for at most ", mcd$kount, 
                 " variables."))
    }
    if(mcd$exactfit == 1) {
      ans$method <- paste(ans$method, 
                          "\nThe covariance matrix of the data is singular.")
    }
    if(mcd$exactfit == 2) {
      ans$method <- paste(ans$method, 
                    "\nThe covariance matrix has become singular during\n",
                    "the iterations of the MCD algorithm.")
    }
    if(p == 2) {
      ans$method <- paste(ans$method, "\nThere are", mcd$kount, 
                    "observations in the entire dataset of\n", n, 
                    "observations that lie on the line with equation\n",
                    round(mcd$coeff[1, 1], digits = 4), 
                    "(x_i1-m_1)+", round(mcd$coeff[1, 2], digits = 4), 
              "(x_i2-m_2)=0 \nwith (m_1,m_2) the mean of these observations.")
    }
    if(p == 3) {
      ans$method <- paste(ans$method, "\nThere are", mcd$kount, 
                    "observations in the entire dataset of\n", n, 
                    "observations that lie on the plane with equation \n",
                    round(mcd$coeff[1, 1], digits = 4), 
                    "(x_i1-m_1)+", round(mcd$coeff[1, 2], digits = 4), 
                    "(x_i2-m_2)+", round(mcd$coeff[1, 3], digits = 4), 
              "(x_i3-m_3)=0 \nwith (m_1,m_2) the mean of these observations.")
    }
    if(p > 3) {
      ans$method <- paste(ans$method, "\nThere are", mcd$kount, 
                    " observations in the entire dataset of\n", n, 
                    "observations that lie on the hyperplane with equation\n",
                    "\na_1*(x_i1-m_1)+...+a_p*(x_ip-m_p)=0 \n",
                    "with (m_1,...,m_p) the mean of\n",
                    "these observations and coefficients a_i equal to: \n")
      for(i in 1:p) {
        ans$method <- paste(ans$method, round(mcd$coeff[1, i], digits = 4))
      } 
    }
  }
  else {
    ans$raw.objective <- mcd$mcdestimate
    weights <- as.vector(mcd$weights)
    tmp <- cov.wt(x, wt = weights, cor)
    ans$cov <- sum(weights)/(sum(weights) - 1) * tmp$cov
    ans$cov <- as.matrix(ans$cov)
    ans$center <- tmp$center
    ans$n.obs  <- tmp$n.obs
    eigs <- eigen(ans$cov, symm=TRUE, only=TRUE)$values
    if(any(eigs < 0)) {
      ans$method <- paste(ans$method, 
                    "\nThe reweighted MCD scatter matrix is singular.") 
    }
  }
  if(print.it)
    cat(ans$method, "\n")
  oldClass(ans) <- "mcd"
  attr(ans, "call") <- sys.call()
  return(ans)
}
