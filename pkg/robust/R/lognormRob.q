lognormRob <- function(x, estim = c("tdmean"),
                       control = lognormRob.control(estim, ...), ...)
{
	estim <- match.arg(estim)
	the.call <- match.call()
  data.name <- deparse(substitute(x))

	beta <- control$beta
	gam <- control$gam
	u <- control$u
	cov	<- control$cov
	tol <- control$tol

	#D.e <- S.D.E.lnorm(x,beta=beta,gam=gam,tol=tol)

	y	<- log(x)
	n <- length(x)
	z	<- .Fortran("rltmadve",
								x = as.double(y),
								n = as.integer(n),
								beta = as.double(beta),
								gam = as.double(gam),
								pos = double(1),
								scal = double(1),
								sx = double(n),
                PACKAGE = "robust")

	Pos	 <- z$pos
	Scal <- z$scal
	alpha <- Pos

	z <- .Fortran("rltrmadn",
								sigma = as.double(1.0),
								beta = as.double(beta),
								gam = as.double(gam),
								tol = as.double(tol),
								sf = double(1),
								isol = integer(1),
                PACKAGE = "robust")

	scal.n <- z$sf
	sigma <- Scal/scal.n
	D.alph <- alpha
	D.sig <- sigma 

	if (z$isol == 0) {
	  zl <- list(mu = NA, alpha = D.alph, sigma = D.sig, Tl = NA,
               Tu = NA, ok = 0, call = the.call)
	  return(zl)
	}

	Dq <- .Fortran("rlquqldl",
									u = as.double(u),
									alpha = as.double(D.alph),
									sigma = as.double(D.sig),
									tol = as.double(tol),
									ql = double(1),
									qu = double(1),
									isol = integer(1),
                  PACKAGE = "robust")

	if(Dq$isol == 0) {
		Dq$ql <- NA
		mu <- NA
	}

	else {
		mnu	<- x[x > Dq$ql & x <= Dq$qu]
    mu <- mean(mnu)
  }

	zl <- list(mu = mu, alpha = D.alph, sigma = D.sig, Tl = Dq$ql, Tu = Dq$qu,
             ok = Dq$isol)

	if(cov) {
		Theta <- S.Theta.lnorm(alpha, sigma, u, beta, gam)

    z <- .Fortran("rlquqldl",
                  u = as.double(u),
									alpha = as.double(alpha),
									sigma = as.double(sigma),
									tol = as.double(tol),
									ql= double(1),
									qu = double(1),
									isol = integer(1),
                  PACKAGE = "robust")

		itc <- 0
		ok <- z$isol

		if(ok != 0) {
			xl <- alpha-10*sigma
			xu <- alpha+10*sigma
			teta  <- unlist(Theta)
			nt <- length(teta)
			til <- 1e-4

      z <- .Fortran("rlavtcml",
                    teta = as.double(teta),
										nt = as.integer(nt),
										alpha = as.double(alpha),
										sigma = as.double(sigma),
										itc = as.integer(itc),
										lower = as.double(xl),
										upper = as.double(xu),
										til = as.double(til),
										sum = double(1),
										iwork = integer(80),
										work = double(320),
                    PACKAGE = "robust")

			zl$V.mu <- z$sum / length(x)
		}
	}

  estimate <- c(zl$alpha, zl$sigma)
  names(estimate) <- c("meanlog", "sdlog")
  sd <- if(!is.null(zl$vcov)) sqrt(diag(zl$vcov)) else c(NA, NA)

  ans <- list(estimate = estimate,
              sd = sd,
              vcov = zl$vcov,
              mu = zl$mu,
              V.mu = zl$V.mu,
              control = control,
              call = the.call,
              densfun = "lnorm",
              data.name = data.name,
              x = x)

  oldClass(ans) <- "fitdstn"
  ans
}


