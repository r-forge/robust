rockem <- function(x, control)
{
	the.call <- match.call()

	n <- nrow(x)
	p <- ncol(x)

## get parameters from the control list ##

	quan <- control$quan
	ntrial <- control$ntrial
	r <- control$r
	alpha <- control$alpha
	tau <- control$tau
	maxit <- control$maxit
	#ntm <- control$ntm
	#ilc <- control$ilc
	#icv <- control$icv
	tol <- control$tol

	if(is.numeric(quan) && 0.5 < quan && quan < 1)
		quan <- ceiling(quan * n)

	if(is.integer(quan)) {
		if(quan < floor((n + p + 1)/2) || quan > n)
			stop("The value for quan must be between (n+p+1)/2 and n")
	}
	else
		quan <- floor((n + p + 1)/2)

## compute the parameters for the M estimator ##

	Rho.param <- function(r,p,n,alpha) {

		eq.rho.b <- function(xc,p,r)
		{

			Edqp <- function(a,p,q) {
				a2 <- a*a
				pchisq(a2,p+q)*2^(q/2)*gamma((p+q)/2)/gamma(p/2)
			}

			xc2 <- xc*xc; xc4 <- xc2*xc2
			rsum <- 0.5*Edqp(xc,p,2)-0.5*Edqp(xc,p,4)/xc2+Edqp(xc,p,6)/(6*xc4)
			rsum <- rsum + xc2*(1-pchisq(xc2,p))/6
			rsum-r*xc2/6
		}

		eq.rho.LWS <- function(xM,p,r)
		{

			Edqp <- function(a,p,q) {
				a2 <- a*a
				pchisq(a2,p+q)*2^(q/2)*gamma((p+q)/2)/gamma(p/2)
			}

			xM2 <- xM*xM; cst0 <- xM2/2
			rsum <- 0.5*Edqp(xM,p,2) + cst0*(1-pchisq(xM2,p))
			rsum-r*cst0
		}

		eq.rho.t <- function(xc,alpha,p,r)
		{

			Edqp <- function(a,p,q) {
				a2 <- a*a
				pchisq(a2,p+q)*2^(q/2)*gamma((p+q)/2)/gamma(p/2)
			}

			xM <- sqrt(qchisq(1-alpha,p)) - xc
			xc2 <- xc*xc; xc4 <- xc2*xc2
			xMpc <- xM+xc; xMpc2 <- xMpc*xMpc
			xM2 <- xM*xM; xM3 <- xM2*xM
			xM4 <- xM2*xM2
			cst0 <- xM2/2 - xM2*(xM4-5*xM2*xc2+15*xc4)/(30*xc4)
			cst2 <- 0.5 + xM4/(2*xc4) - xM2/xc2
			cst3 <- 4*xM/(3*xc2) - 4*xM3/(3*xc4)
			cst4 <- 3*xM2/(2*xc4) - 1/(2*xc2)
			cst5 <- 4*xM/(5*xc4)
			cst6 <- 1/(6*xc4)
			rsum <- 0.5*Edqp(xM,p,2)
			rsum <- rsum + cst0*(pchisq(xMpc2,p)-pchisq(xM2,p))
			rsum <- rsum + cst2*(Edqp(xMpc,p,2)-Edqp(xM,p,2))
			rsum <- rsum + cst3*(Edqp(xMpc,p,3)-Edqp(xM,p,3))
			rsum <- rsum + cst4*(Edqp(xMpc,p,4)-Edqp(xM,p,4))
			rsum <- rsum - cst5*(Edqp(xMpc,p,5)-Edqp(xM,p,5))
			rsum <- rsum + cst6*(Edqp(xMpc,p,5)-Edqp(xM,p,5))
			cst0 <- xM2/2+xc*(5*xc+16*xM)/30
			rsum <- rsum + cst0*(1-pchisq(xMpc2,p))
			rsum-r*cst0
		}

		if(r>(n-p)/(2*n))
			r <- (n-p)/(2*n)

	# alpha <= alf.b

		c1 <- uniroot(eq.rho.b,lower=1,upper=20,p=p,r=r)$root
		alf.b <- 1-pchisq(c1*c1, p)
		if(alpha <= alf.b) 
			return(list(rho="b",r=r,alf.b=alf.b,alf.LWS=NA,c=c1,b0=r*c1*c1/6,M=0))

	# alpha >= alf.LWS

		M1 <- uniroot(eq.rho.LWS,lower=1,upper=12,p=p,r=r)$root
		alf.LWS <- 1-pchisq(M1,p)
		if(alpha >= alf.LWS) 
			return(list(rho="LWS",r=r,alf.b=alf.b,alf.LWS=alf.LWS,c=0,b0=r*M1*M1/2,M=M1))

	# alf.b < alpha < alf.LWS

		sq <- sqrt(qchisq(1-alpha,p))
		up <- sq-0.01
		c2 <- uniroot(eq.rho.t,lower=0.4,upper=up,alpha=alpha,p=p,r=r)$root
		M2 <- sq-c2
		b0 <- r*(M2*M2/2+c2*(5*c2+16*M2)/30)
		list(rho="t",r=r,alf.b=alf.b,alf.LWS=alf.LWS,c=c2,b0=b0,M=M2)
	}

	rckpr <- Rho.param(r, p, n, alpha)
	

  ## compute an initial estimate using the MCD ##

	covmat.mcd <- fastmcd(x, print = FALSE, quan = quan, ntrial = ntrial)
	raw.mcd <- covmat.mcd$raw.cov 

	# raw.mcd is not symmetric (but is very close) #

	raw.mcd[row(raw.mcd) > col(raw.mcd)] <- 0
	raw.mcd <- raw.mcd + t(raw.mcd) - diag(diag(raw.mcd))

  # Parameters removed from control list #

	ntm <- 0
	ilc <- 1
	icv <- 1

  # Allocate memory for rlmcvroc call #

	ATi <- chol(raw.mcd)
	ai <- as.vector(ATi[row(ATi) <= col(ATi)])
	nvar <- p
	ncov <- length(ai)
	mdx <- n
	nobs <- nrow(x)
	nit <- integer(1)
	xk <- double(1)
	dist <- double(nobs)
	sa <- double(ncov)
	st <- double(ncov)
	sr <- double(nvar)
	sd <- double(nvar)

	f.res <- .Fortran("rlmcvroc",
										x = as.double(x),
										a = as.double(ai),
										t = as.double(covmat.mcd$raw.center),
										xc = as.double(rckpr$c),
										xm = as.double(rckpr$M),
										b0 = as.double(rckpr$b0),
										nobs = as.integer(nobs),
										nvar = as.integer(nvar),
										ncov = as.integer(ncov),
										mdx = as.integer(mdx),
										tau = as.double(tau),
										maxit = as.integer(maxit),
										nitmon = as.integer(ntm),
										iloc = as.integer(ilc),
										icnv = as.integer(icv),
										asl = as.double(tol),
										nit = as.integer(nit),
										xk = as.double(xk),
										dist = as.double(dist),
										sa = as.double(sa),
										st = as.double(st),
										sr = as.double(sr),
										sd = as.double(sd),
                    PACKAGE = "robust")

	AT <- matrix(0.0, ncol= nvar, nrow = nvar)
	AT[row(AT) <= col(AT)] <- f.res$a
	AT.A <- AT %*% t(AT)

	list(call = the.call, cov = solve(AT.A), center = f.res$t, raw.cov = raw.mcd,
       raw.center = covmat.mcd$raw.center)
}

