#	Kjell Konis	
#	Robust Library adaptation of S.TD.lnorm
#	03/12/2002

lognormRob.control <- function(estim, alpha1 = 0.5, alpha2 = 20.5,
	u = 0.99, beta = 0.4, gam = 0.4, tol= 1e-4, cov = T)
{
	if(estim == "tdmean")
		control <- list(alpha1 = alpha1, alpha2 = alpha2, u = u,
			beta = beta, gam = gam, tol = tol, cov = cov)

	else
		stop("Argument estim must be \"tdmean\".")

	control
}


lognormRob <- function(data, estim = c("tdmean"), save.data = T,
	control = lognormRob.control(estim, ...), ...)
{
	stop.on.bdObject(data)
	the.call <- match.call()
	estim <- match.arg(estim)
	x <- data

	S.Theta.lnorm <- function(alF, sigF, u=0.99, beta=0.4,gam=0.4) {

		S.TD.fun.l <- function(nfun,...) {
			z <- list(...)
			res <- switch(nfun,
			{
			#(nfun == 1) => S.quql.l(u, alpha, sigma, tol = 0.0001)
				u <- z$u
				alpha <- z$alpha
				sigma <- z$sigma
				tol <- z$tol

				if (is.null(tol))
					tol <- 1e-4

				z <- .Fortran("s_quqldl",
											u = as.double(u),
											alpha = as.double(alpha),
											sigma = as.double(sigma),
											tol = as.double(tol),
											ql = double(1),
											qu = double(1),
											isol = integer(1))

				list(ql = z$ql, qu = z$qu, ok = z$isol)
			}
			,
			{
			#(nfun == 2) => S.G2.n(t, alpha = 0)
				t <- z$t
				alpha <- z$alpha
				if(is.null(alpha))
					alpha <- 0
				z0 <- t-alpha
				tmp <- -(z0+alpha)*dnorm(z0) + pnorm(z0)
				tmp
			}
			,
			{
			#(nfun == 3) => S.G2.l(t, sigma = 1)
				t <- z$t
				sigma <- z$sigma
				if (is.null(sigma))
				sigma <- 1
				t[t<=0] <- 1e-4
				z0 <- (log(t)-sigma^2)/sigma
				I1 <- -z0*dnorm(z0)+pnorm(z0)

			#S.K.n(z0)
				I2 <-  pnorm(z0)

			#S.G.n(z0)
				I3 <-  -dnorm(z0)

				I <-  sigma^2*exp(sigma^2/2)*(I1+sigma^2*I2+2*sigma*I3)
				u0 <- log(t)/sigma

			#S.G.l(t,sigma)
				tmp <- exp(0.5*sigma^2)*(pnorm(u0-sigma))
				1/sigma^3*I- 1/sigma* tmp
			}
			,
			{
			#(nfun == 4) => S.G.n(t,alpha=0)
				t <- z$t
				alpha <- z$alpha
				if(is.null(alpha))
				alpha <- 0
				z0 <- t-alpha
				-dnorm(z0)+alpha*pnorm(z0)
			}
			,
			{
			#(nfun == 5) => S.G.l(t,sigma)
				t <- z$t
				sigma <- z$sigma
				t[t<=0] <- 1e-4 
				u0 <- log(t)/sigma
				tmp <- exp(0.5*sigma^2)*(pnorm(u0-sigma))
				tmp
			}
			,
			{
			#(nfun == 6) => S.K.l(t,sigma)
				t <- z$t
				sigma <- z$sigma
			t[t<=0] <- 1e-4 
				pnorm(log(t)/sigma)
			}
			,
			{
			#(nfun == 7) => S.K1.l(t,sigma)
				t <- z$t
				sigma <- z$sigma
				t[t<=0] <- 1e-4
				dnorm(log(t)/sigma)/(t*sigma)
			}
			,
			{
			#(nfun==8) => S.K2.l(t,sigma)
				t <- z$t
				sigma <- z$sigma
				t[t<=0] <- 1e-4
				z0 <- log(t)/sigma
				tmp <- 1/sigma*(-z0*dnorm(z0))
			}
			)

			res
		}

		iopt  <- 1

		if(abs(beta-0.5) <= 1e-4) {
			beta <- 0.4999
			gam <- 0.4999
			iopt <- 1
		}

	#Theta <- S.Theta.Dsm.l(alF,sigF,u,beta,gam)

	#S.trmean.n(alF,beta)
    mF <- .Fortran("s_trmnn",
										alpha = as.double(alF),
										beta = as.double(beta),
										mf = double(1))$mf

	#S.trmean.n(0,beta)
		m1F <- .Fortran("s_trmnn",
										alpha = as.double(0),
										beta = as.double(beta),
										mf = double(1))$mf

	#S.K1.n(m1F)/sigF
    fm <- dnorm(m1F)/sigF

    tol <- 1e-4

	#S.qad1.n(beta,gam)$qad1*sigF
		D2 <- .Fortran("s_qad1n",
										beta = as.double(beta),
										gam = as.double(gam),
										tol = as.double(tol),
										qad1 = double(1),
										isol = integer(1))$qad1*sigF

	#S.qad1.n(beta,1-gam)$qad1*sigF
    D1 <- .Fortran("s_qad1n",
										beta = as.double(beta),
										gam = as.double(1-gam),
										tol = as.double(tol),
										qad1 = double(1),
										isol = integer(1))$qad1*sigF

	#S.trmadv.n(1,beta,gam)
		s1F <- .Fortran("s_trmadn",
										sigma = as.double(1),
										beta = as.double(beta),
										gam = as.double(gam),
										tol = as.double(tol),
										sf = double(1),
										isol = integer(1))$sf

		QGup1 <- D1+mF
		QGlow1 <- -D1+mF

	#S.K1.n((QGup1-alF)/sigF)/sigF
    fQGup1 <- dnorm((QGup1-alF)/sigF)/sigF

	#S.K1.n((QGlow1-alF)/sigF)/sigF
    fQGlow1 <- dnorm((QGlow1-alF)/sigF)/sigF

    QGup2 <- D2+mF
    QGlow2 <- -D2+mF

	#S.K1.n((QGup2-alF)/sigF)/sigF
    fQGup2 <- dnorm((QGup2-alF)/sigF)/sigF

	#S.K1.n((QGlow2-alF)/sigF)/sigF
    fQGlow2 <- dnorm((QGlow2-alF)/sigF)/sigF

	#S.G.n((QGup2-alF)/sigF)
		A1 <- (S.TD.fun.l(4,t=(QGup2-alF)/sigF)+
						alF*pnorm((QGup2-alF)/sigF)/sigF)*sigF
		A2 <- (S.TD.fun.l(4,t=(QGlow2-alF)/sigF)+
						alF*pnorm((QGlow2-alF)/sigF)/sigF)*sigF
		A3 <- (S.TD.fun.l(4,t=(QGup1-alF)/sigF)+
						alF*pnorm((QGup1-alF)/sigF)/sigF)*sigF
		A4 <- (S.TD.fun.l(4,t=(QGlow1-alF)/sigF)+
						alF*pnorm((QGlow1-alF)/sigF)/sigF)*sigF

	#S.K.n((QGup2-alF)/sigF)
		B1 <- pnorm((QGup2-alF)/sigF)

	#S.K.n((QGlow2-alF)/sigF)
		B2 <- pnorm((QGlow2-alF)/sigF)

	#S.K.n((QGup1-alF)/sigF)
		B3 <- pnorm((QGup1-alF)/sigF)

	#S.K.n((QGlow1-alF)/sigF)
		B4 <- pnorm((QGlow1-alF)/sigF)

	#S.trmadv.n(sigF,beta,gam)
		sF <- .Fortran("s_trmadn",
										sigma = as.double(sigF),
										beta = as.double(beta),
										gam = as.double(gam),
										tol = as.double(tol),
										sf = double(1),
										isol = integer(1))$sf

		uF <- qnorm(1-beta)*sigF+alF
		lF <- qnorm(beta)*sigF+alF
		W2beta <- (1-2*beta)*mF+beta*lF+beta*uF

	#Derivatives of M

		tmp1 <- (uF-alF)/sigF
		tmp2 <- (lF-alF)/sigF

	#dnorm((uF-alF)/sigF)/dnorm((uF-alF)/sigF)
		upF <- 1

	#dnorm((lF-alF)/sigF)/dnorm((lF-alF)/sigF)
		lpF <- 1
		MpF <- tmp1*dnorm(tmp1)*(upF)+S.TD.fun.l(2,t=tmp1) -
              (tmp2*dnorm(tmp2)*(lpF)+S.TD.fun.l(2,t=tmp2))
		MpF <- MpF/(1-2*beta)

	#Derivative of D(1-gam)=D2
		A <- (m1F+D2/sigF)
		B <- (m1F-D2/sigF)
		dnormA <- dnorm(A)
		dnormB <- dnorm(B)
		D2pF <- (1-MpF)*(dnormA-dnormB)
		D2pF <- D2pF/(dnormA+dnormB)
 
	#Derivative of sF(1-gam)
		S2pF <- A*dnormA*(MpF+D2pF)+S.TD.fun.l(2,t=A) +
							B*dnormB*(MpF-D2pF)+S.TD.fun.l(2,t=B) -
							mF*( dnormA*(MpF+D2pF-1) +
							dnormB*(MpF-D2pF-1)) - MpF*(B1+B2)

	#Derivative of D(gam)=D1
		A <- (m1F+D1/sigF)
		B <- (m1F-D1/sigF)
		D1pF <- (1-MpF)*(dnormA-dnormB)
		D1pF <- D1pF/(dnormA+dnormB)
     
	#Derivative of sF(gam)
		S1pF<- A*dnormA*(MpF+D1pF)+S.TD.fun.l(2,t=A)+
							B*dnormB*(MpF-D1pF)+S.TD.fun.l(2,t=B)-
							m1F*( dnormA*(MpF+D1pF-1) +
							dnormB*(MpF-D1pF-1)) - MpF*(B3+B4)

	#Derivative of S=S2-S1
		SpF <- (S2pF-S1pF)/(1-2*gam)

		mu1F <- exp(0.5*sigF^2)
		muF <- exp(alF+0.5*sigF^2)

	#S.quql.l(u,0,sigF)
		z <- S.TD.fun.l(1,u=u,alpha=0,sigma=sigF)

		qu1F <- z$qu
		ql1F <- z$ql

	#S.quql.l(u,alF,sigF)
		z <- S.TD.fun.l(1,	u	=	u,	alpha	=	alF,	sigma	=	sigF)

		quF <- z$qu
		qlF <- z$ql

	#S.K1.l(quF/exp(alF),sigF)/exp(alF)
		fquF <- S.TD.fun.l(7,t=quF/exp(alF), sigma=sigF)/exp(alF)

	#S.K1.l(qlF/exp(alF),sigF)/exp(alF)
		fqlF <- S.TD.fun.l(7,t=qlF/exp(alF), sigma=sigF)/exp(alF)

	#S.K.l(qlF/exp(alF),sigF)
		FqlF <- S.TD.fun.l(6,t=qlF/exp(alF), sigma=sigF)

	#S.H0.l(u,alF,sigF)
		H0 <- u
		xsqu <- exp(sigF*qnorm(u))
		expalF <- exp(alF)

	#S.H1.l(u,alF,sigF)
		H1 <- expalF*S.TD.fun.l(5,t=xsqu,sigma=sigF)

	#S.J0.l(u,alF,sigF)
		J0 <- S.TD.fun.l(6,t=qlF/expalF,sigma=sigF)

	#S.J1.l(u,alF,sigF)
		J1 <- expalF*S.TD.fun.l(5,t=qlF/expalF,sigma=sigF)

	#S.K.l(ql1F,sigF)
		Kl1 <- S.TD.fun.l(6,t=ql1F, sigma=sigF)

	#S.K1.l(ql1F,sigF)
		K1l1 <- S.TD.fun.l(7,t=ql1F, sigma=sigF)

	#S.K1.l(qu1F,sigF)
		K1u1 <- S.TD.fun.l(7,t=qu1F, sigma=sigF)

	#S.K2.l(qu1F,sigF)
		K2u1 <- S.TD.fun.l(8,t=qu1F, sigma=sigF)

	#S.K2.l(ql1F,sigF)
		K2l1 <- S.TD.fun.l(8,t=ql1F, sigma=sigF)

	#S.G1.l(qu1F,sigF)
		G1u1 <- qu1F*dlnorm(qu1F,0,sigF)

	#S.G1.l(ql1F,sigF)
		G1l1 <- ql1F*dlnorm(ql1F,0,sigF)

	#S.G2.l(qu1F,sigF)
		G2u1 <- S.TD.fun.l(3,t=qu1F, sigma=sigF)

	#S.G2.l(ql1F,sigF)
		G2l1 <- S.TD.fun.l(3,t=ql1F, sigma=sigF)

  	Theta <- list(iopt = iopt, alF = alF, sigF = sigF, beta = beta,
									gam = gam, mF = mF, m1F = m1F, D1 = D1, D2 = D2,
									sF = sF, s1F = s1F, uF = uF, lF = lF, W2beta = W2beta,
									A1 = A1, A2 = A2, A3 = A3, A4 = A4, B1 = B1, B2 = B2,
									B3 = B3, B4 = B4, QGup1 = QGup1, QGlow1 = QGlow1,
									QGup2 = QGup2, QGlow2 = QGlow2, fm = fm, fQGup1 = fQGup1,
									fQGlow1 = fQGlow1, fQGup2 = fQGup2, fQGlow2 = fQGlow2,
									MpF = MpF, SpF = SpF, u = u, mu1F = mu1F, muF = muF,
									qu1F = qu1F, quF = quF, ql1F = ql1F, qlF = qlF,
									fquF = fquF, fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1,
									J0 = J0, J1 = J1, Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1,
									K2u1 = K2u1, K2l1 = K2l1, G1u1 = G1u1, G1l1 = G1l1,
									G2u1 = G2u1, G2l1 = G2l1, D2pF = D2pF, D1pF = D1pF,
									S1pF = S1pF, S2pF = S2pF)

		Theta
	}


	beta <- control$beta
	gam <- control$gam
	u <- control$u
	cov	<- control$cov
	tol <- control$tol

	#D.e <- S.D.E.lnorm(x,beta=beta,gam=gam,tol=tol)

	y	<- log(x)
	n <- length(x)
	z	<- .Fortran("s_tmadve",
								x = as.double(y),
								n = as.integer(n),
								beta = as.double(beta),
								gam = as.double(gam),
								pos = double(1),
								scal = double(1),
								sx = double(n))

	Pos	 <- z$pos
	Scal <- z$scal
	alpha <- Pos
	sig <- 1
	z <- .Fortran("s_trmadn",
								sigma = as.double(sig),
								beta = as.double(beta),
								gam = as.double(gam),
								tol = as.double(tol),
								sf = double(1),
								isol = integer(1))

	scal.n <- z$sf
	sigma <- Scal/scal.n
	D.alph <- alpha
	D.sig <- sigma 

	if (z$isol == 0) {
	  zl <- list(mu = NA, alpha = D.alph, sigma = D.sig, Tl = NA,
								Tu = NA, ok = 0, call = the.call)
	  return(zl)
	}

	Dq <- .Fortran("s_quqldl",
									u = as.double(u),
									alpha = as.double(D.alph),
									sigma = as.double(D.sig),
									tol = as.double(tol),
									ql = double(1),
									qu = double(1),
									isol = integer(1))

	if (Dq$isol == 0) {
		Dq$ql <- NA
		mu <- NA
	}

	else
	#mu <- S.tcmean.E(x, Dq$ql, Dq$qu)
		mnu	<- x[x > Dq$ql & x <= Dq$qu]; mu <- mean(mnu)

	zl <- list(mu = mu, alpha = D.alph, sigma = D.sig, Tl = Dq$ql,
							Tu = Dq$qu, ok = Dq$isol)

	if(cov) {
		Theta <- S.Theta.lnorm(alpha, sigma, u, beta, gam)

		z <- .Fortran("s_quqldl",
									u = as.double(u),
									alpha = as.double(alpha),
									sigma = as.double(sigma),
									tol = as.double(tol),
									ql= double(1),
									qu = double(1),
									isol = integer(1))

		itc <- 0
		ok <- z$isol
		if(ok != 0) {
			xl <- alpha-10*sigma
			xu <- alpha+10*sigma
			teta  <- unlist(Theta)
			nt <- length(teta)
			til <- 1e-4

			z <- .Fortran("s_avtcml",
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
										work = double(320))

			zl$V.mu <- z$sum / length(x)
		}
	}

	if(save.data)
		zl$data <- data

	zl$call <- the.call
	zl$header <- "Robust lognormal distribution parameter estimate"
	zl$plot.header <- "Robust Estimate of Lognormal Density"
	zl$density.fn <- dlnorm
	zl$quantile.fn <- qlnorm
	oldClass(zl) <- "lognormRob"
	zl
}


lognormMLE <- function(data, save.data = T)
{
  # Estimate parameters of a lognormal distribution.
  # Take a log transform and estimate parameters for that normal distn

  y <- log(data)
  meany <- mean(y)
  sigma2 <- var(y)

  # Calculate mu = estimated mean of the lognormal distn

  mu <- exp(meany + 0.5 * sigma2)

  # V.mu is the variance of estimate "mu"

	V.mu <- (mu^2 * sigma2 * (1 + sigma2/2)) / (length(y) - 1)

  ans <- list(call = match.call(),
              alpha = meany,
              sigma = sqrt(sigma2),
              mu = mu,
              n = length(data),
              V.mu = V.mu)

  if(save.data)
    ans$data <- data

	ans$header <- "MLE lognormal distribution parameter estimate"
	ans$plot.header <- "MLE Estimate of Lognormal Density"
	ans$density.fn <- dlnorm
	ans$quantile.fn <- qlnorm
  oldClass(ans) <- "lognormMLE"
  ans
}







