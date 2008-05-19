# wblrob.s
# Matias
# 09/08/00


weibullRob.control <- function(estim, b1 = 1.5, b2= 1.7, A = c(0,0,0),
	maxit = 100, tol = 1e-4, til = 1e-3, sigma = 0, alpha1 = 0.5,
	alpha2 = 20.5, u = 0.99, beta = 0.4, gam = 0.4, cov = T)
{
	if(estim == "M")
		control <- list(maxit = maxit, b1 = b1, b2 = b2, A = A, tol = tol,
			til = til, cov = cov, sigma = sigma)

	else if(estim == "tdmean")
		control <- list(alpha1 = alpha1, alpha2 = alpha2, u = u,
			beta = beta, gam = gam, cov = cov, tol = tol)

	else stop("Argument estim must be one of \"tdmean\" or \"M\".")

	control
}


weibullRob <- function(data, estim = c("tdmean", "M"), save.data = T,
	control = weibullRob.control(estim, ...), ...)
{
	stop.on.bdObject(data)
	the.call <- match.call()
	estim <- match.arg(estim)

	Tab.weibull <- function(b1 = 1.5, b2= 1.7, A = c(0,0,0), monit = 0,
		maxta = 1, maxtc = 1, maxit = 30, til = 0.001, tol = 0.001)
	{
		call <- match.call()
		clb <- call$b1

		if (is.null(clb))
			call$b1 <- 1.5
		else if (!is.numeric(clb))
			call$b1 <- eval(clb, local=sys.parent(1)) 
		clb <- call$b2

		if (is.null(clb))
			call$b2 <- 1.5
		else if (!is.numeric(clb))
			call$b2 <- eval(clb, local=sys.parent(1))

		if (abs(b1-b2) < 1e-5 & b1 < 1.075)
			cat("Solution for b1=b2 & b1<1.075 might not exist!\n")

		if (monit!=0) {
			cat("Alfa,  Nit,  f(c1),  f(c2), fa(1),  fa(2), fa(3) \n")
			cat("nsol,  x2(1),  x2(2),  x2(3),  x2(4):\n")
		}

		f.res <- .Fortran("s_cretabw",
											b1=as.double(b1),
											b2=as.double(b2),
											a=as.double(A),
											maxta=as.integer(maxta),
											maxtc=as.integer(maxtc),
											maxit=as.integer(maxit),
											til=as.double(til),
											tol=as.double(tol),
											monit=as.integer(monit),
											tab=double(5),
											tpar=double(6))

		f.res$tab <- array(f.res$tab, c(NULL,5) )
		dimnames(f.res$tab) <- list(c("c1*v","c2*v","a11/v","a21/v","a22/v"))
		list(Table=f.res$tab, call=call)
	}

	if(estim == "M") {
		y <- data
		maxit <- control$maxit
		tol <- control$tol
		til <- control$til
		sigma <- control$sigma
		cov <- control$cov
		b1 <- control$b1
		b2 <- control$b2
		A <- control$A
		Table <- Tab.weibull(b1 = b1, b2 = b2, A = A, maxit = maxit,
							til = til, tol = tol)
		nobs <- length(y)
		y[y==0] <- 0.5
		y <- sort(y)
		tab <- Table$Table
		ct <- Table$call
		alf1 <- 0.4 

		if(!is.null(ct$alpha1))
			alf1 <- ct$alpha1 

		alf2 <- 10.4    
  
		if(!is.null(ct$alpha2))
			alf2 <- ct$alpha2 

		k <- 1
		tpar <- c(ct$b1, ct$b2, alf1, alf2, k, alf2-alf1)
		y <- log(y)

		f.res <- .Fortran("s_westim",
											y=as.double(y),
											nobs=as.integer(nobs),
											tab=as.double(tab),
											maxit=as.integer(maxit),
											tpar=as.double(tpar),
											tol=as.double(tol),
											alfa1=as.double(alf1),
											alfa2=as.double(alf2),
											alpha=double(1),
											sigma=as.double(sigma),
											nit=integer(1),
											c1c2=double(2),
											a123=double(3))

		alpha <- f.res$alpha
		sigma <- f.res$sigma 
		mu <- gamma(1+1/alpha)*sigma 
		zl <- list(alpha = alpha, sigma = sigma, mu = mu, ok = f.res$nit)


		if (cov) {
			f.cov <- .Fortran("s_varweb",
												alpha=as.double(alpha),
												sigma=as.double(sigma),
												tab=as.double(tab),
												tpar=as.double(tpar),
												til=as.double(til),
												m=double(4),
												q=double(4),
												mi=double(4),
												v=double(4),
												vsiga=double(4),
												vmoy=double(1))

			cov <- matrix(f.cov$vsiga, nrow=2)
			V.mu <- f.cov$vmoy
			dn <- list(c("sigma","alfa"),c("sigma","alfa"))
			dimnames(cov) <-  list(c("sigma","alfa"),c("sigma","alfa"))
			zl$cov <- cov / length(y)
			zl$V.mu <- V.mu / length(y)
		}
	}

	else if(estim == "tdmean") {
		x <- data

		S.Theta.weibull <- function(shape, scale, u = 0.99, beta = 0.4, gam = 0.4)
		{
			S.TD.fun.w <- function(nfun, ...) {
				z <- list(...)
				res <- switch(nfun,
				{
				#(nfun == 1) => S.quql.w(u, alpha, sigma, tol = 0.0001)
					u <- z$u
					alpha <- z$alpha
					sigma <- z$sigma
					tol <- z$tol; if (is.null(tol)) tol <- 1e-4
					z <- .Fortran("s_quqldw",
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
				#(nfun == 2) => S.qad1.w(alpha, beta, gam, tol = 0.0001)
					alpha <- z$alpha
					beta <- z$beta
					gam <- z$gam
					tol <- z$tol

					if (is.null(tol))
						tol <- 1e-4

						z <- .Fortran("s_qad1w",
												alpha = as.double(alpha),
												beta = as.double(beta),
												gam = as.double(gam),
												tol = as.double(tol),
												qad1 = double(1),
												isol = integer(1))

					list( qad1 = z$qad1, ok = z$isol)
				}
				,
				{
				#(nfun == 3) => S.dlweib(y, sigma=1, alpha=1)
					y <- z$y
					sigma <- z$sigma
					alpha <- z$alpha

		 			if(is.null(sigma))
						sigma <- 1

					if(is.null(alpha))
						alpha <- 1

					tau <- log(sigma)
					v <- 1/alpha
					t <- (y-tau)/v

					(1/v)*exp(t-exp(t))
				}
				,
				{
				#(nfun == 4) => S.K.lw(t, sigma = 1, alpha = 1)
					t <- z$t
					sigma <- z$sigma
					alpha <- z$alpha

					if(is.null(sigma))
						sigma <- 1

					if (is.null(alpha))
						alpha <- 1

					y <- exp(t)
					pweibull(y, alpha, sigma)
				}
				,
				{
				#(nfun == 5) => S.G.w(t,alpha)
					t <- z$t
					alpha <- z$alpha
					a1 <- 1+1/alpha
					a2 <- 2+1/alpha
					ga1 <- gamma(a1)
					p <- pweibull(t,alpha)
					mup <- p*ga1
					lp <- log(1/(1-p))

					for(k in 1:length(p)) {
						if(p[k] == 1)
							break

						pa <- 1/alpha[k]+1
						ig <- .Fortran("s_ingama",
														x = as.double(lp[k]),
														p = as.double(pa),
														g = double(1))$g

						mup[k] <- ga1[k]*ig
					}
					mup
				}
				,
				{
				#(nfun == 6) => S.K2.w(t, alpha)
					t <- z$t
					alpha <- z$alpha
					z0 <- t^alpha
					one <- 1
					two <- 2

				#S.Intlgam(t, alpha)
					tmp1 <- .Fortran("s_sumlgm",
														hi = as.double(z0),
														alpha = as.double(one),
														gl = double(1))$gl

				#S.Intlgam(t, alpha)
					tmp2 <- .Fortran("s_sumlgm",
														hi = as.double(z0),
														alpha = as.double(two),
														gl = double(1))$gl

					tmp <- (1/alpha)*(pweibull(t,alpha)+tmp1-gamma(2)*tmp2)
					tmp
				}
				,
				{
				#(nfun == 7) => S.G2.w(t, alpha)
					t <- z$t
					alpha <- z$alpha
					z0 <- t^alpha
					a1 <- 1+1/alpha
					a2 <- 2+1/alpha
					ga1 <- gamma(a1)
					p <- pweibull(t,alpha)
					mup <- p*ga1
					lp <- log(1/(1-p))

					for(k in 1:length(p)){
						if(p[k]==1)
							break

						pa <- 1/alpha[k]+1

						ig <- .Fortran("s_ingama",
														x = as.double(lp[k]),
														p = as.double(pa),
														g = double(1))$g

						mup[k] <- ga1[k]*ig
					}

					tmp1 <- .Fortran("s_sumlgm",
														hi = as.double(z0),
														alpha = as.double(a1),
														gl = double(1))$gl
  
					tmp2 <- .Fortran("s_sumlgm",
														hi = as.double(z0),
														alpha = as.double(a2),
														gl = double(1))$gl
    
					tmp  <- (1/alpha)*(mup + ga1*tmp1 - gamma(a2)*tmp2)
					tmp
				}
				,
				{
				#(nfun == 8) => S.G.lw(t, sigma = 1)
					t <- z$t
					sigma <- z$sigma

					if(is.null(sigma))
						sigma <- 1

					lsg <- log(sigma)
					et <- exp(t-lsg)
					alpha <- 1

				#S.Intlgam(et, 1)
					z <- .Fortran("s_sumlgm",
												hi = as.double(et),
												alpha = as.double(alpha),
												gl = double(1))$gl

					z+lsg*(1-exp(-et))
				}
				)
			res
			}

			if(abs(beta-0.5)<=1e-4) {
			#Theta <- S.Theta.D.w(shape,scale,u)

				mu1F <- gamma(1+1/shape)
				muF <- scale*gamma(1+1/shape)

			#S.med.w(shape,scale)
				medF <- scale*qweibull(0.5,shape)

				tmp <- S.TD.fun.w(2,alpha=shape,beta=0.5,gam=0.5)$qad1

			#S.mad.w(shape,scale)
				madF <- scale*tmp

			#S.med.w(shape,1)
				M1F <- qweibull(0.5,shape)

			#S.mad.w(shape,1)
				D1F <- tmp

			#S.K1.w(medF/scale,shape)/scale
				fmed <- dweibull(medF/scale,shape)/scale

			#S.K1.w(M1F+D1F,shape)/scale
				fMpD <- dweibull(M1F+D1F,shape)/scale

			#S.K1.w(M1F-D1F,shape)/scale
				fMmD <- dweibull(M1F-D1F,shape)/scale

				MpF <- -S.TD.fun.w(6,t=M1F,alpha=shape)/dgamma(M1F,shape)  
				A <- M1F+D1F
				B <- M1F-D1F
				DpF <- S.TD.fun.w(6,t=B,alpha=shape)-S.TD.fun.w(6,t=A,alpha=shape)-
								MpF*(dweibull(A,shape)-dweibull(B,shape))
				DpF <- DpF/(dweibull(A,shape)+dweibull(B,shape))     

			#S.quql.w(u,shape,1)
				z <- S.TD.fun.w(1,u=u,alpha=shape,sigma=1)

				qu1F <- z$qu
				ql1F <- z$ql

			#S.quql.w(u,shape,scale)
				z <- S.TD.fun.w(1,u=u,alpha=shape,sigma=scale)

				quF <- z$qu
				qlF <-  z$ql
				fquF <- dweibull(quF/scale,shape)/scale
				fqlF <- dweibull(qlF/scale,shape)/scale

			#S.K.w(ql1F,shape)
				FqlF <- pweibull(ql1F,shape)

			#S.H0.w(u,shape,scale)
				H0 <- u
				tmp <- qweibull(u,shape,1)

			#S.H1.w(u,shape,scale)
				H1 <- scale*S.TD.fun.w(5,t=tmp,alpha=shape)

			#S.J0.w(u,shape,scale)
				J0 <- pweibull(ql1F,shape)

			#S.J1.w(u,shape,scale)
				J1 <- scale*S.TD.fun.w(5,t=ql1F,alpha=shape)

			#S.K.w(ql1F,shape)
				Kl1 <- pweibull(ql1F,shape)

				K1l1 <- dweibull(ql1F,shape)
				K1u1 <- dweibull(qu1F,shape)

			#S.K2.w(qu1F,shape)
				K2u1 <- S.TD.fun.w(6,t=qu1F,alpha=shape)

				K2l1 <- S.TD.fun.w(6,t=ql1F,alpha=shape)

			#S.G1.w(qu1F,shape)
				G1u1 <- qu1F*dweibull(qu1F,shape)

			#S.G1.w(ql1F,shape)
				G1l1 <- ql1F*dweibull(ql1F,shape)

			#S.G2.w(qu1F,shape)
				G2u1 <- S.TD.fun.w(7,t=qu1F,alpha=shape)

			#S.G2.w(ql1F,shape)
				G2l1 <- S.TD.fun.w(7,t=ql1F,alpha=shape)

		   Theta <- list(iopt = 2, u = u, mu1F = mu1F, muF = muF, qu1F = qu1F,
											quF = quF, ql1F = ql1F, qlF = qlF, fquF = fquF,
											fqlF = fqlF, FqlF = FqlF, H0 = H0, H1 = H1, J0 = J0,
											J1 = J1, Kl1 = Kl1, K1l1 = K1l1, K1u1 = K1u1, K2u1 = K2u1,
											K2l1 = K2l1, G1u1 = G1u1, G1l1 = G1l1, G2u1 = G2u1,
											G2l1 = G2l1, alF = shape, sigF = scale, M1F = M1F,
											D1F = D1F, MpF = MpF, DpF = DpF, medF = medF, madF = madF,
											fmed = fmed, fMpD = fMpD, fMmD = fMmD)
			}

			else {
			#Theta <- S.Theta.Dsm.w(shape,scale,u,beta,gam)

				tau <- log(scale)
				v <- 1/shape
				tol <- 1e-4

				mF <- .Fortran("s_trmnlw",
												alpha = as.double(shape),
												sigma = as.double(scale),
												beta = as.double(beta),
												mf = double(1))$mf

			#z <- S.trmadv.lw(1,beta,gam)
				one <- 1
				z <- .Fortran("s_trmadlw",
												alpha = as.double(one),
												beta = as.double(beta),
												gam = as.double(gam),
												tol = as.double(tol),
												mf = double(1),
												sf = double(1),
												isol = integer(1))

				m1F <- z$mf
				s1F <- z$sf

			#S.K1.lw(m1F)/v
				fm <- S.TD.fun.w(3,y=m1F,sigma=1,alpha=1)/v

			#S.qad1.lw(beta,gam)$qad1/shape
				D2 <- .Fortran("s_qad1lw",
												beta = as.double(beta),
												gam = as.double(gam),
												tol=  as.double(tol),
												qad1 = double(1),
												isol = integer(1))$qad1/shape

			#S.qad1.lw(beta,1-gam)$qad1/shape
				D1<- .Fortran("s_qad1lw",
												beta = as.double(beta),
												gam = as.double(1-gam),
												tol = as.double(tol),
												qad1 = double(1),
												isol = integer(1))$qad1/shape

				QGup1 <- D1+mF
				QGlow1 <- -D1+mF
				tmp <- (QGup1-tau)/v
				fQGup1 <- S.TD.fun.w(3,y=tmp,sigma=1,alpha=1)/v#S.K1.lw((QGup1-tau)/v)/v
				tmp <- (QGlow1-tau)/v
				fQGlow1 <- S.TD.fun.w(3,y=tmp,sigma=1,alpha=1)/v#S.K1.lw((QGlow1-tau)/v)/v
				QGup2 <- D2+mF
				QGlow2 <- -D2+mF
				tmp <- (QGup2-tau)/v
				fQGup2 <- S.TD.fun.w(3,y=tmp,sigma=1,alpha=1)/v#S.K1.lw((QGup2-tau)/v)/v
				tmp <- (QGlow2-tau)/v
				fQGlow2 <- S.TD.fun.w(3,y=tmp,sigma=1,alpha=1)/v#S.K1.lw((QGlow2-tau)/v)/v  

			#S.K.lw((QGup2-tau)/v)
				B1 <- S.TD.fun.w(4,t=(QGup2-tau)/v)

			#S.K.lw((QGlow2-tau)/v)
				B2 <- S.TD.fun.w(4,t=(QGlow2-tau)/v)

			#S.K.lw((QGup1-tau)/v)
				B3 <- S.TD.fun.w(4,t=(QGup1-tau)/v)

			#S.K.lw((QGlow1-tau)/v)
				B4 <- S.TD.fun.w(4,t=(QGlow1-tau)/v)

			#S.G.lw((QGup2-tau)/v)+tau*B1/v)*v
				A1  <-  (S.TD.fun.w(8,t=(QGup2-tau)/v)+tau*B1/v)*v

				A2 <- (S.TD.fun.w(8,t=(QGlow2-tau)/v)+tau*B2/v)*v
				A3 <- (S.TD.fun.w(8,t=(QGup1-tau)/v)+tau*B3/v)*v
				A4 <- (S.TD.fun.w(8,t=(QGlow1-tau)/v)+tau*B4/v )*v

			#S.trmadv.lw(shape,beta,gam)$sF
				sF <- .Fortran("s_trmadlw",
												alpha=as.double(shape),
												beta=as.double(beta),
												gam=as.double(gam),
												tol=as.double(tol),
												mf=double(1),
												sf=double(1),
												isol=integer(1))$sf

				uF <- log(qweibull(1-beta,1))*v+tau
				lF <- log(qweibull(beta,1))*v+tau
				W2beta <- (1-2*beta)*mF+beta*lF+beta*uF

				mu1F <- gamma(1+1/shape)
				muF <- scale*gamma(1+1/shape) 

			#S.quql.w(u,shape,1)
				z <- S.TD.fun.w(1,u=u,alpha=shape,sigma=1)
				qu1F <- z$qu
				ql1F <- z$ql

			#S.quql.w(u,shape,scale)
				z <- S.TD.fun.w(1,u=u,alpha=shape,sigma=scale)
				quF <- z$qu
				qlF <-  z$ql

			#S.K1.w(qu1F,shape)/scale
				fquF <- dweibull(qu1F,shape)/scale

			#S.K1.w(ql1F,shape)/scale
				fqlF <- dweibull(ql1F,shape)/scale

			#S.K.w(ql1F,shape)
				FqlF  <- pweibull(ql1F,shape)

			#S.H0.w(u,shape,scale)
				H0 <- u
				tmp <- qweibull(u,shape,1)

			#S.H1.w(u,shape,scale)
				H1 <- scale*S.TD.fun.w(5,t=tmp,alpha=shape)

			#S.J0.w(u,shape,scale)
				J0 <- pweibull(ql1F,shape)

			#S.J1.w(u,shape,scale)
				J1 <- scale*S.TD.fun.w(5,t=ql1F,alpha=shape)

			#S.K.w(ql1F,shape)
				Kl1 <- pweibull(ql1F,shape)

			#S.K1.w(ql1F,shape)
				K1l1 <- fqlF*scale

			#S.K1.w(qu1F,shape)
				K1u1 <- fquF*scale

			#S.K2.w(qu1F,shape)
				K2u1 <- S.TD.fun.w(6,t=qu1F,alpha=shape)

			#S.K2.w(ql1F,shape)
				K2l1 <- S.TD.fun.w(6,t=ql1F,alpha=shape)

			#S.G1.w(qu1F,shape)
				G1u1 <- qu1F*dweibull(qu1F,shape)

			#S.G1.w(ql1F,shape)
				G1l1 <- ql1F*dweibull(ql1F,shape)

			#S.G2.w(qu1F,shape)
				G2u1 <- S.TD.fun.w(7,t=qu1F,alpha=shape)

			#S.G2.w(ql1F,shape)
				G2l1 <- S.TD.fun.w(7,t=ql1F,alpha=shape)

				MpF <- 0
				SpF <- 0

				Theta <- list(iopt = 1, alF = shape, sigF = scale, beta = beta,
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
											G2u1 = G2u1, G2l1 = G2l1)
			}
			Theta
		}  

		u <- control$u
		beta <- control$beta
		gam <- control$gam
		cov <- control$cov
		tol <- control$tol
		n <- length(x)
		logx <- log(x)

		z <- .Fortran("s_tmadve",
									x = as.double(logx),
									n = as.integer(n),
									beta = as.double(beta),
									gam = as.double(gam),
									pos = double(1),
									scal = double(1),
									sx = double(n))

		Pos <- z$pos
		Scal <- z$scal
		alsg <- 1
   
		zlw <- .Fortran("s_trmadlw",
										alpha = as.double(alsg),
										beta = as.double(beta),
										gam = as.double(gam),
										tol = as.double(tol),
										mf = double(1),
										sf = double(1),
										isol = integer(1))

		pos.lw <- zlw$mf
		scal.lw <- zlw$sf
		v <- Scal/scal.lw
		tau <- Pos-pos.lw*v
		scale <- exp(tau)
		shape <- 1/v
		ok <- zlw$isol
		D.shap <- shape
		D.scal <- scale

		if(ok == 0) {
			zl <- list(alpha = D.shap, sigma = D.scal, mu = NA,
							Tl = NA, Tu = NA, ok = 0, call = the.call)
			return(zl)
		}

		Dq <- .Fortran("s_quqldw",
										u = as.double(u),
										alpha = as.double(D.shap),
										sigma = as.double(D.scal),
										tol = as.double(tol),
										ql = double(1),
										qu = double(1),
										isol = integer(1))

		if(Dq$isol == 0) {
			Dq$ql <- NA
			mu <- NA
		}

		else
		#mu <- S.tcmean.E(x, Dq$ql, Dq$qu)
			mnu <- x[x > Dq$ql & x <= Dq$qu]; mu <- mean(mnu)

		zl <- list(alpha = D.shap, sigma = D.scal, mu = mu,
						Tl = Dq$ql, Tu = Dq$qu, ok = Dq$isol)

		if(cov) {
		#xmax <- limit.w(D.shap,D.scal)

			shape <- D.shap
			scale <- D.scal
			tl <- 1e-10
			mu <- gamma(1+1/shape)*scale
			lim <- 5*mu
			dw <- dweibull(lim,shape,scale)

			if(is.na(dw)) {
				lim <- mu
				mu <- 10/shape
				dw <- dweibull(lim,shape,scale)
			}

			repeat {
				if(dw < tl)
					break
				lim <- lim + mu
				dw <- dweibull(lim,shape,scale)
				if (is.na(dw)) {
					lim <- lim-mu
					break
				}
			}

			xmax <- lim
			Theta <- S.Theta.weibull(shape, scale, u, beta, gam)

			#z <- S.quql.w(u,D.shap,1); ok <- z$ok

			one <- 1
			z <- .Fortran("s_quqldw",
										u = as.double(u),
										alpha = as.double(D.shap),
										sigma = as.double(one),
										tol = as.double(tol),
										ql = double(1),
										qu = double(1),
										isol=  integer(1))

			ok <- z$isol

			itc <- 0
			if(ok != 0 | itc != 0) {
				teta  <- unlist(Theta)
				nt <- length(teta)
				til <- 1e-4

				z <- .Fortran("s_avtcmw",
											teta = as.double(teta),
											nt = as.integer(nt),
											alpha = as.double(D.shap),
											sigma = as.double(D.scal),
											itc = as.integer(itc),
											upper = as.double(xmax),
											til = as.double(til),
											sum = double(1),
											iwork = integer(80),
											work = double(320))

				zl$V.mu  <- z$sum / length(x)
			}
		} 
	}

	zl$call <- the.call
	if(save.data)
		zl$data <- data
	zl$density.fn <- dweibull
	zl$quantile.fn <- qweibull
	zl$header <- "Robust weibull distribution parameter estimate"
	zl$plot.header <- "Robust Estimate of Weibull Density"
	oldClass(zl) <- "weibullRob"
	zl
}


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







