# File gamrob.q
#
# 21/03/00
#  9/08/00
# 21/08/00

gammaRob.control <- function(estim, b1 = 1.5, b2 = 1.7, k = 101,
	A = c(0, 0, 0), maxta = 1, maxtc = 1, maxit = 100, tol = 1e-4,
	til = 1e-3, sigma = 0, alpha1 = 0.5, alpha2 = 20.5, u = 0.99,
	beta = 0.4, gam = 0.4, cov = T)
{
	if(estim == "M")
		control <- list(b1 = b1, b2 = b2, k = k, A = A, maxta = maxta,
								maxtc = maxtc, maxit = maxit, tol = tol, til = til,
								cov = cov, sigma = sigma)

	else if(estim == "tdmean")
		control <- list(alpha1 = alpha1, alpha2 = alpha2, u = u,
								beta = beta, gam = gam, cov = cov, tol = tol)

	else
		stop("Argument estim must be one of \"tdmean\" or \"M\".")

	control
}


gammaRob <- function(data, estim = c("tdmean", "M"),
	save.data = T, control = gammaRob.control(estim, ...),
	...)
{
	stop.on.bdObject(data)
	estim <- match.arg(estim)
	the.call <- match.call()

	if(estim == "M") {
		y <- data

		Tabgamma <- function(b1 = 1.5, b2 = 1.7, alpha1 = 0.5, alpha2 = 20.5,
			k = 101, A = c(0, 0, 0), monit = 0, maxta = 1, maxtc = 1, maxit = 100, 
			til = 0.001, tol = 0.001)
		{
			fun.call <- match.call()
			clb <- fun.call$b1

			if(is.null(clb))
				fun.call$b1 <- 1.5
			else if(!is.numeric(clb))
				fun.call$b1 <- eval(clb, local=sys.parent(1))

			clb <- fun.call$b2

			if (is.null(clb))
				fun.call$b2 <- 1.5
			else if (!is.numeric(clb))
				fun.call$b2 <- eval(clb, local=sys.parent(1))

			clb <- fun.call$alpha1

			if (is.null(clb))
				fun.call$alpha1 <- 0.5
			else if (!is.numeric(clb))
				fun.call$alpha1 <- eval(clb, local=sys.parent(1)) 

			clb <- fun.call$alpha2

			if (is.null(clb))
				fun.call$alpha2 <- 20.5
			else if (!is.numeric(clb))
				fun.call$alpha2 <- eval(clb, local=sys.parent(1))

			clb <- fun.call$k

			if (is.null(clb))
				fun.call$k <- 101
			else if (!is.numeric(clb))
				fun.call$k <- eval(clb, local=sys.parent(1))

			A.type <- 2
			tab <- matrix(single(1), nrow=k, ncol=5)

			if (monit!=0) {
				cat("Alfa,  Nit,  f(c1),  f(c2), fa(1),  fa(2), fa(3) \n")
				cat("nsol,  x2(1),  x2(2),  x2(3),  x2(4):\n")
			}

			til <- tol

			f.res <- .Fortran("s_cretabi",
												b1=as.double(b1),
												b2=as.double(b2),
												kk=as.integer(k),
												la=as.integer(A.type),
												a=as.double(A),
												maxta=as.integer(maxta),
												maxtc=as.integer(maxtc),
												maxit=as.integer(maxit),
												til=as.double(til),
												tol=as.double(tol),
												alpha1=as.double(alpha1),
												alpha2=as.double(alpha2),
												monit=as.integer(monit),
												tab=as.double(tab),
												tpar=as.double(rep(0,6)))

			f.res$tab <- matrix(f.res$tab, nrow = k, ncol = 5)
			dimnames(f.res$tab) <- list(NULL, c("c1","c2","a11","a21","a22"))
			list(Table = f.res$tab, call = fun.call)
		}

		maxit <- control$maxit
		maxta <- control$maxta
		maxtc <- control$maxtc
		tol <- control$tol
		til <- control$til
		sigma <- control$sigma
		cov <- control$cov
		b1  <- control$b1
		b2  <- control$b2
		k <- control$k
		A <- control$A
		shape <- median(y)^2 / mad(y)^2
		scale <- mad(y)^2 / median(y)
		alfa1 <- max(.2, shape-2*scale)
		alfa2 <- shape+2*scale
		Table <- Tabgamma(alpha1 = alfa1, alpha2 = alfa2, b1 = b1, b2 = b2,
							k = k, A = A, maxta = maxta, maxtc = maxtc, maxit = maxit,
							til = til, tol = tol)
		nobs <- length(y)
		y[y==0] <- 0.5
		y <- sort(y)
		tab <- Table$Table
		ct <- Table$call
		alf1 <- ct$alpha1

		if (is.null(alf1))
			alf1 <- 0.5

		alf2 <- ct$alpha2

		if (is.null(alf2))
			alf2 <- 20.5

		k <- ct$k

		if (is.null(k))
			k <- 101

		tpr6 <- if (k <=1) 0 else (alf2-alf1)/(k-1)

		tpar <- c(ct$b1, ct$b2, alf1, alf2, k, tpr6) 
		a.typ <- 2
		mdt <- nrow(tab)

		f.res <- .Fortran("s_estimp",
											y=as.double(y),
											nobs=as.integer(nobs),
											tab=as.double(tab),
											mdt=as.integer(mdt),
											la=as.integer(a.typ),
											maxit=as.integer(maxit),
											tpar=as.double(tpar),
											tol=as.double(tol),
											alfa1=as.double(alf1),
											alfa2=as.double(alf2),
											alpha=as.double(1),
											sigma=as.double(sigma),
											nit=as.integer(1),
											c1c2=as.double(rep(0,2)),
											a123=as.double(rep(0,3)))

		falf <- f.res$alpha
		fsig <- f.res$sigma 
		zl<- list(alpha = falf, sigma = fsig, mu = falf*fsig, ok=f.res$nit)

		if(cov) {
			alpha <- falf 
			sigma <- fsig
			f.cov <- .Fortran("s_vargam",
												mdt=as.integer(mdt),
												alpha=as.double(alpha),
												sigma=as.double(sigma),
												tab=as.double(tab),
												tpar=as.double(tpar),
												til=as.double(til),
												m=as.double(rep(0,4)),
												q=as.double(rep(0,4)),
												mi=as.double(rep(0,4)),
												v=as.double(rep(0,4)),
												vsiga=as.double(rep(0,4)),
												vmoy=as.double(rep(0,1)),
												message = as.integer(0))

			zc <- f.cov$vsiga
			cov <- matrix(f.cov$vsiga, nrow=2)
			V.mu <- f.cov$vmoy
			dn <- list(c("tau","alfa"),c("tau","alfa"))
			dimnames(cov) <- list(c("sigma","alfa"), c("sigma","alfa"))
			zl$cov <- cov
			zl$V.mu <- V.mu
			zl$cov <- cov / length(y)
			zl$V.mu <- V.mu / length(y)
		}
	}

	else if(estim == "tdmean") {
		x <- data

		S.Theta.gamma <- function(alF, sigF, u = 0.99, beta = 0.4, gam = 0.4) {

			S.TD.fun.g <- function(nfun, ...) {
				z <- list(...)
				res <- switch(nfun,
				{
				#(nfun == 1) => S.quql.g(u, alpha, sigma, tol = 0.0001)
					u <- z$u
					alpha <- z$alpha
					sigma <- z$sigma
					tol <- z$zol
					if(is.null(tol))
						tol <- 1e-4

					z <- .Fortran("s_quqldg",
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
				#(nfun == 2) => S.qad1.g(alpha, beta, gam, tol = 0.0001)
					alpha <- z$alpha
					beta <- z$beta
					gam <- z$gam
					tol <- z$zol
					if(is.null(tol))
						tol <- 1e-4

					z <- .Fortran("s_qad1dg",
												alpha = as.double(alpha),
												beta = as.double(beta),
												gam = as.double(gam),
												tol = as.double(tol),
												qad1 = double(1),
												isol = integer(1))

					list(qad1 = z$qad1, ok = z$isol)
				}
				,
				{
				#(nfun == 3) => S.Intlgam(upper, alpha)
					upper <- z$upper
					alpha <- z$alpha

				## Int ln(x)*gamma(alpha,x) dx, for x=0 to upper.

					f.res <- .Fortran("s_sumlgm",
													hi = as.double(upper),
													alpha = as.double(alpha),
													gl = double(1))

					f.res$gl
				}
				,
				{
				#(nfun == 4) => S.ingama(x, p)
					x <- z$x
					p <- z$p

					f.res <- .Fortran("s_ingama",
														x = as.double(x),
														p = as.double(p),
														g = double(1))
					f.res$g
				}
				,
				{
				#(nfun == 5) => S.digama(s) 
					s <- z$s 

					f.res <- .Fortran("s_digama",
														s = as.double(s),
														result = double(1))
					f.res$result
				}
				,
				{
				#(nfun==6) => S.K2.g(t,alpha)
					t <- z$t
					alpha <- z$alpha

				#S.Intlgam(t,alpha)
					tmp1 <- .Fortran("s_sumlgm",
														hi=as.double(t),
														alpha=as.double(alpha),
														gl=double(1))$gl

				#S.digama(alpha)
					tmp2 <- .Fortran("s_digama",
														s=as.double(alpha),
														result=double(1))$result

					tmp1 - tmp2*pgamma(t, alpha)
				}
				,
				{
				#(nfun == 7) => S.G2.g(t,alpha)
					t <- z$t
					alpha <- z$alpha

				#S.Intlgam(t,alpha+1)
					tmp1 <- .Fortran("s_sumlgm",
														hi = as.double(t),
														alpha = as.double(alpha+1),
														gl = double(1))$gl

				#S.digama(alpha+1)
					tmp2 <- .Fortran("s_digama",
														s=as.double(alpha+1),
														result=double(1))$result

				#S.K2.g(t,alpha+1) 
					tmp  <- tmp1 - tmp2*pgamma(t, alpha+1)
					pgamma(t,(alpha+1))+alpha*tmp
				}
				) # end of switch

				res
			}

			if (abs(beta-0.5)<=1e-4) {
			#Theta <- S.Theta.D.g(alF,sigF,u)    
				mu1F <- alF
				muF <- alF*sigF

			#S.med.g(alF,sigF)
				medF <- sigF*qgamma(0.5,alF)

			#S.mad.g(alF,sigF)
				madF <- sigF*S.TD.fun.g(2,alpha=alF,beta=0.5,gam=0.5)$qad1

			#S.med.g(alF,1)
				M1F <- qgamma(0.5,alF)

			#S.mad.g(alF,1)
				D1F <- S.TD.fun.g(2,alpha=alF,beta=0.5,gam=0.5)$qad1

			#S.K1.g(M1F,alF)/sigF
				fmed <- dgamma(M1F,alF)/sigF

			#S.K1.g(M1F+D1F,alF)/sigF
				fMpD <- dgamma(M1F+D1F,alF)/sigF

			#S.K1.g(M1F-D1F,alF)/sigF
				fMmD <- dgamma(M1F-D1F,alF)/sigF

			#-S.K2.g(M1F,alF)/S.K1.g(M1F,alF)
				MpF <- -S.TD.fun.g(6, t = M1F, alpha = alF) / dgamma(M1F, alF)
				A <- M1F+D1F
				B <- M1F-D1F

			#DpF  <- S.K2.g(B,alF)-S.K2.g(A,alF)-MpF*(S.K1.g(A,alF)-S.K1.g(B,alF))
			#DpF  <- DpF/(S.K1.g(A,alF)+S.K1.g(B,alF))
	
				DpF <- S.TD.fun.g(6, t = B, alpha = alF) -
								S.TD.fun.g(6,t=A,alpha=alF) -
								MpF*(dgamma(A,alF) -
								dgamma(B,alF))

				DpF <- DpF/(dgamma(A,alF)+dgamma(B,alF))
				z1 <- S.TD.fun.g(1,u=u,alpha=alF,sigma=1)
				qu1F <- z1$qu
				ql1F <- z1$ql	
				zq <- S.TD.fun.g(1,u=u,alpha=alF,sigma=sigF)
				quF <- zq$qu
				qlF <- zq$q

			#S.K1.g(quF/sigF,alF)/sigF
				fquF <- dgamma(quF/sigF,alF)/sigF

			#S.K1.g(qlF/sigF,alF)/sigF
				fqlF <- dgamma(qlF/sigF,alF)/sigF

			#S.K.g(ql1F,alF)
				FqlF <- pgamma(ql1F,alF)

			#S.H0.g(u,alF,sigF)
				H0 <- u
				qu1 <- qgamma(u,alF)

			#S.H1.g(u,alF,sigF)
				H1 <- alF*sigF*pgamma(qu1,(alF+1))

			#ql1 <- S.quql.g(u,alF,1)$ql
			#S.J0.g(u,alF,sigF)
				J0 <- pgamma(ql1F,alF)

			#ql1 <- S.quql.g(u,alF,1)$ql
			#S.J1.g(u,alF,sigF)
				J1 <- alF*sigF*pgamma(ql1F,(alF+1))

			#S.K.g(ql1F,alF)
				Kl1 <- pgamma(ql1F,alF)

			#S.K1.g(ql1F,alF)
				K1l1 <- dgamma(ql1F,alF)

			#S.K1.g(qu1F,alF)
				K1u1 <- dgamma(qu1F,alF)
				K2u1 <- S.TD.fun.g(6,t=qu1F,alpha=alF)
				K2l1 <- S.TD.fun.g(6,t=ql1F,alpha=alF)

			#S.G1.g(qu1F,alF)
				G1u1 <- qu1F*dgamma(qu1F,alF)

			#S.G1.g(ql1F,alF)
				G1l1 <- ql1F*dgamma(ql1F,alF)
				G2u1 <- S.TD.fun.g(7,t=qu1F,alpha=alF)
				G2l1 <- S.TD.fun.g(7,t=ql1F,alpha=alF)

				Theta <- list(iopt = 2, u = u, mu1F = mu1F, muF = muF,
							qu1F = qu1F, quF = quF, ql1F = ql1F,qlF = qlF,
							fquF = fquF, fqlF = fqlF, FqlF = FqlF, H0 = H0,
							H1 = H1, J0 = J0, J1 = J1, Kl1 = Kl1, K1l1 = K1l1,
							K1u1 = K1u1, K2u1 = K2u1, K2l1 = K2l1, G1u1 = G1u1,
							G1l1 = G1l1, G2u1 = G2u1, G2l1 = G2l1, alF = alF,
							sigF = sigF, M1F = M1F, D1F = D1F, MpF = MpF,
							DpF = DpF, medF = medF, madF = madF, fmed = fmed,
							fMpD = fMpD, fMmD = fMmD)
			}
			
			else {

			#Theta <- S.Theta.Dsm.g(alF,sigF,u,beta,gam)

				mu1F  <- alF
				muF <- alF*sigF
				tmg <- .Fortran("s_trmng",
												alpha = as.double(alF),
												sigma = as.double(sigF),
												beta = as.double(beta),
												mf = double(1))

				mF <- tmg$mf 
				m1F <- mF/sigF

			#S.K1.g(mF/sigF,alF)/sigF
				fm <- dgamma(mF/sigF,alF)/sigF

		  	D2 <- S.TD.fun.g(2,alpha=alF,beta=beta,gam=gam)$qad1*sigF
				D1 <- S.TD.fun.g(2,alpha=alF,beta=beta,gam=1-gam)$qad1*sigF
				QGup1 <- D1+mF
				QGlow1 <- -D1+mF

			#S.K1.g(QGup1/sigF,alF)/sigF
				fQGup1 <- dgamma(QGup1/sigF,alF)/sigF

			#S.K1.g(QGlow1/sigF,alF)/sigF
				fQGlow1 <- dgamma(QGlow1/sigF,alF)/sigF

				QGup2 <- D2+mF
				QGlow2 <- -D2+mF

			#S.K1.g(QGup2/sigF,alF)/sigF
				fQGup2 <- dgamma(QGup2/sigF,alF)/sigF

			#S.K1.g(QGlow2/sigF,alF)/sigF
				fQGlow2 <- dgamma(QGlow2/sigF,alF)/sigF

			#S.G.g(QGup2/sigF, alF)*sigF
				A1 <- alF*pgamma(QGup2/sigF,(alF+1))*sigF

			#S.G.g(QGlow2/sigF,alF)*sigF
				A2 <- alF*pgamma(QGlow2/sigF,(alF+1))*sigF

			#S.G.g(QGup1/sigF, alF)*sigF
				A3 <- alF*pgamma(QGup1/sigF,(alF+1))*sigF

			#S.G.g(QGlow1/sigF,alF)*sigF
				A4 <- alF*pgamma(QGlow1/sigF,(alF+1))*sigF

			#S.K.g(QGup2/sigF,alF)
				B1 <- pgamma(QGup2/sigF,alF)

			#S.K.g(QGlow2/sigF,alF)
				B2 <- pgamma(QGlow2/sigF,alF)

			#S.K.g(QGup1/sigF,alF)
				B3 <- pgamma(QGup1/sigF,alF)

			#S.K.g(QGlow1/sigF,alF)
				B4 <- pgamma(QGlow1/sigF,alF)

				sF <- ((A1+A2-A3-A4)-mF*(B1+B2-B3-B4))/(1-2*gam)
				uF <- qgamma(1-beta,alF)*sigF
				lF <- qgamma(beta,alF)*sigF
				W2beta <- (1-2*beta)*mF+beta*lF+beta*uF

			#Derivatives of M
				upF <- -S.TD.fun.g(6,t=uF/sigF,alpha=alF)/dgamma(uF/sigF,alF)
				lpF <- -S.TD.fun.g(6,t=lF/sigF,alpha=alF)/dgamma(lF/sigF,alF)
				MpF <- (uF/sigF)*dgamma((uF/sigF),alF)*(upF)+S.TD.fun.g(7,t=uF/sigF,alpha=alF)-
								((lF/sigF)*dgamma((lF/sigF),alF)*(lpF)+S.TD.fun.g(7,t=lF/sigF,alpha=alF))
				MpF <- MpF/(1-2*beta)

			#Derivative of D(1-gam) = D2
				A <- (mF+D2)/sigF
				B <- (mF-D2)/sigF
				D2pF <- S.TD.fun.g(6,t=B,alpha=alF)-S.TD.fun.g(6,t=A,alpha=alF) -
							MpF*(dgamma(A,alF)-dgamma(B,alF))
				D2pF <- D2pF/(dgamma(A,alF)+dgamma(B,alF))

			#Derivative of sF(1-gam)                        

				S2pF <- A*dgamma(A,alF)*(MpF+D2pF)+S.TD.fun.g(7,t=A,alpha=alF)+
								B*dgamma(B,alF)*(MpF-D2pF)+S.TD.fun.g(7,t=B,alpha=alF)-
								m1F*(dgamma(A,alF)*(MpF+D2pF)+S.TD.fun.g(6,t=A,alpha=alF)+
								dgamma(B,alF)*(MpF-D2pF)+S.TD.fun.g(6,t=B,alpha=alF))-
								MpF*(B1+B2)

			#Derivative of D(gam) = D1
				A <- (mF+D1)/sigF
				B <- (mF-D1)/sigF
				D1pF <- S.TD.fun.g(6,t=B,alpha=alF)-S.TD.fun.g(6,t=A,alpha=alF) -
									MpF*(dgamma(A,alF)-dgamma(B,alF))
				D1pF <- D1pF/(dgamma(A,alF)+dgamma(B,alF))

			#Derivative of sF(gam)
				S1pF <- A*dgamma(A,alF)*(MpF+D1pF)+S.TD.fun.g(7,t=A,alpha=alF)+
								B*dgamma(B,alF)*(MpF-D1pF)+S.TD.fun.g(7,t=B,alpha=alF)-
								m1F*(dgamma(A,alF)*(MpF+D1pF)+S.TD.fun.g(6,t=A,alpha=alF)+
								dgamma(B,alF)*(MpF-D1pF)+S.TD.fun.g(6,t=B,alpha=alF))-
								MpF*(B3+B4)

			#Derivative of S=S2-S1
				SpF <- (S2pF-S1pF)/(1-2*gam) 

				z <- S.TD.fun.g(1,u=u,alpha=alF,sigma=1)
				qu1F <- z$qu
				ql1F <- z$ql
				z <- S.TD.fun.g(1,u=u,alpha=alF,sigma=sigF)
				quF <- z$qu
				qlF <- z$ql
				fquF <- dgamma(quF/sigF,alF)/sigF
				fqlF <- dgamma(qlF/sigF,alF)/sigF

			#S.K.g(ql1F,alF)
				FqlF <- pgamma(ql1F,alF)

			#S.H0.g(u,alF,sigF)
				H0 <- u

				qu1 <- qgamma(u,alF) 

			#S.H1.g(u,alF,sigF)
				H1 <- alF*sigF*pgamma(qu1,(alF+1))

			#S.J0.g(u,alF,sigF)
				J0 <- pgamma(ql1F,alF)

			#S.J1.g(u,alF,sigF)
				J1 <- alF*sigF*pgamma(ql1F,(alF+1))

			#S.K.g(ql1F,alF)
				Kl1  <- pgamma(ql1F,alF)

				K1l1 <- dgamma(ql1F,alF)
				K1u1 <- dgamma(qu1F,alF)
				K2u1 <- S.TD.fun.g(6,t=qu1F,alpha=alF)
				K2l1 <- S.TD.fun.g(6,t=ql1F,alpha=alF)

			#S.G1.g(qu1F,alF)
				G1u1 <- qu1F*dgamma(qu1F,alF)

			#S.G1.g(ql1F,alF)
				G1l1 <- ql1F*dgamma(ql1F,alF)

				G2u1 <- S.TD.fun.g(7,t=qu1F,alpha=alF)
				G2l1 <- S.TD.fun.g(7,t=ql1F,alpha=alF)     

				Theta <- list(iopt = 1, alF = alF, sigF = sigF, beta = beta,
											gam = gam, mF = mF, m1F = m1F, D1 = D1, D2 = D2,
											sF = sF, s1F = 1, uF = uF, lF = lF, W2beta = W2beta,
											A1 = A1, A2 = A2, A3 = A3, A4 = A4, B1 = B1, B2 = B2,
											B3 = B3, B4 = B4, QGup1 = QGup1, QGlow1 = QGlow1,
											QGup2 = QGup2, QGlow2 = QGlow2, fm = fm, fQGup1 = fQGup1,
											fQGlow1 = fQGlow1, fQGup2 = fQGup2, fQGlow2 = fQGlow2,
											MpF = MpF, SpF = SpF, u = u, mu1F = mu1F, muF = muF,
											qu1F = qu1F, quF = quF, ql1F = ql1F, qlF = qlF, fquF = fquF,
											fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1, J0 = J0, J1 = J1,
											Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1, K2u1 = K2u1, K2l1 = K2l1,
											G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1, G2l1 = G2l1,
											D2pF = D2pF, D1pF = D1pF, S1pF = S1pF, S2pF = S2pF)
			}

			Theta
		}

		alpha1 <- control$alpha1
		alpha2 <- control$alpha2
		beta <- control$beta
		gam <- control$gam
		cov <- control$cov

		if(is.null(cov))
			cov <- F

		tol <- control$tol

		if(is.null(tol))
			tol <- 1e-4   

		n <- length(x)

		z <- .Fortran("s_tmadve",
									x = as.double(x),
									n = as.integer(n),
									beta = as.double(beta),
									gam = as.double(gam),
									pos = double(1),
									scal = double(1),
									sx = double(n))

		Pos <- z$pos
		Scal <- z$scal  

		al <- .Fortran(	"s_solvdg",
										pos = as.double(Pos),
										scal = as.double(Scal),
										beta = as.double(beta),
										gam = as.double(gam),
										alfa1 = as.double(alpha1),
										alfa2 = as.double(alpha2),
										tol = as.double(tol),
										alpha = double(1),
										isol = integer(1))

		if(al$isol == 0) {
			cat(" WARNING: No solution in the interval [ alpha1 =",alpha1,
				",alpha2 =", alpha2,"]\n")
			zl <- list(alpha = NA, sigma = 1, mu = NA, m = Pos, s = Scal, ok = 0)
			return(zl)
		}

		alpha <- al$alpha
		sigma <- 1

		tmg <- .Fortran("s_trmng",
										alpha = as.double(alpha),
										sigma = as.double(sigma),
										beta = as.double(beta),
										mf = double(1))

		sigma <- Pos/tmg$mf
		mu <- alpha*sigma
		ok <- al$isol; if (abs(Scal)<=1e-6) ok <- 0
		zl <- list(alpha = alpha, sigma = sigma, mu = mu, m = Pos,
						s = Scal, ok= ok)

		u <- control$u
		Dq <- .Fortran("s_quqldg",
										u=as.double(u),
										alpha=as.double(alpha),
										sigma=as.double(sigma),
										tol=as.double(tol),
										ql=double(1),
										qu=double(1),
										isol=integer(1))

		ok <- Dq$isol

		if(ok == 0) {
			Dq$ql <- NA
			mu <- NA
		}

	#
	#	What do they actually want to do with this line?
	#

		else
			mnu <- x[x > Dq$ql & x <= Dq$qu]; mu <- mean(mnu)

		zl <- list(mu = mu, alpha = alpha, sigma = sigma, Tl = Dq$ql,
						Tu = Dq$qu, ok = ok)

		if(cov) {
			est  <- 0
			tl <- 1e-10
			stl <- sigma*tl
			mu  <- alpha*sigma
			lim <- 5*mu
			repeat {
				if(dgamma(lim/sigma, alpha) < stl) break
				lim <- lim + mu
			}

			xmax <- lim
			Theta <- S.Theta.gamma(alpha,sigma,u,beta,gam)
			one <- 1
			z <- .Fortran("s_quqldg",
										u = as.double(u),
										alpha = as.double(alpha),
										sigma = as.double(one),
										tol = as.double(tol),
										ql = double(1),
										qu = double(1),
										isol = integer(1))

			ok <- z$isol
			itc <- 0
			if(ok != 0 | itc != 0) {
				teta  <- unlist(Theta)
				nt <- length(teta)
				til <- 1e-4
				z <- .Fortran("s_avtcmg",
											teta = as.double(teta),
											nt = as.integer(nt),
											alpha = as.double(alpha),
											sigma = as.double(sigma),
											itc = as.integer(itc),
											upper = as.double(xmax),
											til = as.double(til),
											sum = double(1),
											iwork = integer(80),
											work = double(320))

	     zl$V.mu <- z$sum / length(x)
			}
		}
	}

	else
		stop("Invalid Estimator")

	if(save.data)
		zl$data <- data

	zl$call <- the.call
	zl$header <- "Robust gamma distribution parameter estimate"
	zl$plot.header <- "Robust Estimate of Gamma Density"
	zl$density.fn <- dgamma
	zl$quantile.fn <- qgamma
	oldClass(zl) <- "gammaRob"
	zl
}


gammaMLE.control <- function(maxit = 100, tol = 1e-3, cov = T)
{
	list(maxit = maxit, tol = tol, cov = cov)
}


gammaMLE <- function(data, save.data = T,
							control = gammaMLE.control(...), ...) 
{	
	y <- data
	maxit <- control$maxit
	tol <- control$tol
	cov <- control$cov
	the.call <- match.call()
	nobs <- length(y)

	f.res <- .Fortran("s_gamlik",
										y=as.double(y),
										nobs=as.integer(nobs),
										maxit=as.integer(maxit),
										tol=as.double(tol),
										alpha=as.double(0),
										sigma=as.double(0),
										ybar=as.double(0),
										var=as.double(0),
										zero=as.double(0),
										nit=as.integer(0))

	falf <- f.res$alpha
	fsig <- f.res$sigma 
	zl <- list(alpha = falf, sigma = fsig, mu = falf*fsig, nit = f.res$nit)

	if(save.data)
		zl$data <- y

	if (cov) {
		alpha <- zl$alpha
		sigma <- zl$sigma
		Delt <- alpha*trigamma(alpha) - 1
		cov <- matrix(rep(-sigma,4), nrow=2)
		cov[1,1] <- sigma^2*trigamma(alpha)
		cov[2,2] <- alpha
		cov <- cov / Delt
		dimnames(cov) <- list(c("sigma","alfa"), c("sigma","alfa"))
		thet <- matrix(c(alpha,sigma), ncol = 1)
		mu <- t(thet)%*%cov%*%thet
		zl$V.mu <- as.vector(mu)
		zl$cov <- cov
		zl$cov <- zl$cov / length(y)
		zl$V.mu <- zl$V.mu / length(y)
	}

	zl$call <- the.call
	zl$header <- "MLE gamma distribution parameter estimate"
	zl$plot.header <- "MLE Estimate of Gamma Density"
	zl$density.fn <- dgamma
	zl$quantile.fn <- qgamma
	oldClass(zl) <- "gammaMLE"
	zl
}

