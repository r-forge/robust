# wblrob.s
# Matias
# 09/08/00





weibullMLE.control <- function(maxit = 100, tol = 1e-3, cov = T)
{
	list(maxit = maxit, tol = tol, cov = cov)
}


weibullMLE <- function(data, save.data = T, control = weibullMLE.control(...), ...)
{
	y <- data
	maxit <- control$maxit
	tol <- control$tol
	cov <- control$cov
	the.call <- match.call()
	nobs <- length(y)

	f.res <- .Fortran("s_weilik",
										sy=as.double(y),
										nobs=as.integer(nobs),
										maxit=as.integer(maxit),
										tol=as.double(tol),
										alpha=double(1),
										sigma=double(1),
										zero=double(1),
										nit=integer(1))

	alf <- f.res$alpha
	sig <- f.res$sigma
	mu <- gamma(1+1/alf)*sig 
	zl <- list(alpha = alf, sigma = sig, mu = mu, nit = f.res$nit)

	if(cov) {
		alf2 <- 1/(alf^2) 
		dg2 <- 0.4227843351 #S.digama(2)
		tg2 <- pi^2/6 - 1   #trigam(2) 
		diag <- -dg2/sig
		cov <- matrix(rep(diag,4), nrow=2)
		cov[1,1] <- (alf/sig)^2
		cov[2,2] <- alf2 + tg2*alf2 + (dg2/alf)^2
		cov <- solve(cov)
		dimnames(cov) <- list(c("sigma", "alfa"), c("sigma", "alfa"))
		thet <- matrix(c(mu/sig,-mu*digamma(1+1/alf)*alf2),ncol=1)
		V.mu <- t(thet)%*%cov%*%thet
		V.mu <- as.vector(V.mu)
		zl$cov <- cov / length(y)
		zl$V.mu <- V.mu  / length(y)
	}

	if(save.data)
		zl$data <- data

	zl$call <- the.call
	zl$density.fn <- dweibull
	zl$quantile.fn <- qweibull
	zl$header <- "MLE weibull distribution parameter estimate"
	zl$plot.header <- "MLE Estimate of Weibull Density"
	oldClass(zl) <- "weibullMLE"
	zl
}





