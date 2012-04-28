gammaMLE <- function(data, save.data = TRUE, maxit = 100, tol = 1e-3,
                     cov = TRUE) 
{	
	y <- data
	the.call <- match.call()
	nobs <- length(y)

	f.res <- .Fortran("rlgamlik",
										y = as.double(y),
										nobs = as.integer(nobs),
										maxit = as.integer(maxit),
										tol = as.double(tol),
										alpha = as.double(0),
										sigma = as.double(0),
										ybar = as.double(0),
										var = as.double(0),
										zero = as.double(0),
										nit = as.integer(0),
                    PACKAGE = "robust")

	falf <- f.res$alpha
	fsig <- f.res$sigma 
	zl <- list(alpha = falf, sigma = fsig, mu = falf*fsig, nit = f.res$nit)

	if(save.data)
		zl$data <- y

	if(cov) {
		alpha <- zl$alpha
		sigma <- zl$sigma
		Delt <- alpha * trigamma(alpha) - 1
		cov <- matrix(rep(-sigma, 4), nrow = 2)
		cov[1,1] <- sigma^2 * trigamma(alpha)
		cov[2,2] <- alpha
		cov <- cov / Delt
		dimnames(cov) <- list(c("sigma","alpha"), c("sigma","alpha"))
		thet <- matrix(c(alpha, sigma), ncol = 1)
		mu <- t(thet) %*% cov %*% thet
		#zl$V.mu <- as.vector(mu)
		#zl$cov <- cov
		zl$V.mu <- as.vector(mu) / length(y)
		zl$cov <- cov / length(y)
	}

	zl$call <- the.call
	zl$header <- "MLE gamma distribution parameter estimate"
	zl$plot.header <- "MLE Estimate of Gamma Density"
	zl$density.fn <- substitute(dgamma)
	zl$quantile.fn <- substitute(qgamma)
	oldClass(zl) <- c("gammaMLE", "asmDstn")
	zl
}


