gammaRob <- function(data, estim = c("tdmean", "M"), save.data = TRUE,
                     control = gammaRob.control(estim, ...), ...)
{
	estim <- match.arg(estim)
	the.call <- match.call()

	if(estim == "M") {
		y <- data

		Tabgamma <- function(b1 = 1.5, b2 = 1.7, alpha1 = 0.5, alpha2 = 20.5,
                         k = 101, A = c(0, 0, 0), maxta = 1, maxtc = 1,
                         maxit = 100, til = 0.001, tol = 0.001)
		{
			tab <- matrix(0.0, nrow = k, ncol = 5)
      storage.mode(tab) <- "double"

			f.res <- .Fortran("rlcretabi",
												b1 = as.double(b1),
												b2 = as.double(b2),
												kk = as.integer(k),
												la = as.integer(2),
												a = as.double(A),
												maxta = as.integer(maxta),
												maxtc = as.integer(maxtc),
												maxit = as.integer(maxit),
												til = as.double(til),
												tol = as.double(tol),
												alpha1 = as.double(alpha1),
												alpha2 = as.double(alpha2),
												monit = as.integer(0),
												tab = tab,
												tpar = double(6),
                        PACKAGE = "robust")

			tab <- f.res$tab
			dimnames(tab) <- list(NULL, c("c1", "c2", "a11", "a21", "a22"))
			tab
		}

		maxit <- control$maxit
		maxta <- control$maxta
		maxtc <- control$maxtc
		tol <- control$tol
		til <- control$til
		sigma <- control$sigma
		cov <- control$cov
		b1 <- control$b1
		b2 <- control$b2
		k <- control$k
		A <- control$A
		shape <- median(y)^2 / mad(y)^2
		scale <- mad(y)^2 / median(y)
		alpha1 <- max(.2, shape - 2.0 * scale)
		alpha2 <- shape + 2.0 * scale

		tab <- Tabgamma(b1, b2, alpha1, alpha2, k, A, maxta, maxtc, maxit, til, tol)
		mdt <- nrow(tab)
		nobs <- length(y)

		y[y == 0] <- 0.5
		y <- sort(y)

    tpr6 <- ifelse(k <= 1, 0, (alpha2 - alpha1) / (k - 1))
		tpar <- c(b1, b2, alpha1, alpha2, k, tpr6) 

		f.res <- .Fortran("rlestimp",
											y = as.double(y),
											nobs = as.integer(nobs),
											tab = as.double(tab),
											mdt = as.integer(mdt),
											la = as.integer(2),
											maxit = as.integer(maxit),
											tpar = as.double(tpar),
											tol = as.double(tol),
											alpha1 = as.double(alpha1),
											alpha2 = as.double(alpha2),
											alpha = as.double(1),
											sigma = as.double(sigma),
											nit = as.integer(1),
											c1c2 = double(2),
											a123 = double(3),
                      PACKAGE = "robust")

    alpha <- f.res$alpha
		sigma <- f.res$sigma 
		zl <- list(alpha = alpha, sigma = sigma, mu = alpha*sigma, ok = f.res$nit)

		if(cov) {
			f.cov <- .Fortran("rlvargam",
												mdt = as.integer(mdt),
												alpha = as.double(alpha),
												sigma = as.double(sigma),
												tab = as.double(tab),
												tpar = as.double(tpar),
												til = as.double(til),
												m = double(4),
												q = double(4),
												mi = double(4),
												v = double(4),
												vsiga = double(4),
												vmoy = double(1),
												message = as.integer(0),
                        PACKAGE = "robust")

			zc <- f.cov$vsiga
			cov <- matrix(f.cov$vsiga, nrow = 2)
			V.mu <- f.cov$vmoy
			dimnames(cov) <- list(c("sigma", "alpha"), c("sigma", "alpha"))
			#zl$cov <- cov
			#zl$V.mu <- V.mu
			zl$cov <- cov / length(y)
			zl$V.mu <- V.mu / length(y)
		}
	}

	else if(estim == "tdmean") {
		x <- data

		alpha1 <- control$alpha1
		alpha2 <- control$alpha2
		beta <- control$beta
		gam <- control$gam
		cov <- control$cov

		if(is.null(cov))
			cov <- FALSE

		tol <- control$tol

		if(is.null(tol))
			tol <- 1e-4   

		n <- length(x)

		z <- .Fortran("rltmadve",
									x = as.double(x),
									n = as.integer(n),
									beta = as.double(beta),
									gam = as.double(gam),
									pos = double(1),
									scal = double(1),
									sx = double(n),
                  PACKAGE = "robust")

		Pos <- z$pos
		Scal <- z$scal  

		al <- .Fortran("rlsolvdg",
                   pos = as.double(Pos),
                   scal = as.double(Scal),
                   beta = as.double(beta),
                   gam = as.double(gam),
                   alfa1 = as.double(alpha1),
                   alfa2 = as.double(alpha2),
                   tol = as.double(tol),
                   alpha = double(1),
                   isol = integer(1),
                   PACKAGE = "robust")

		if(al$isol == 0) {
			warning("no solution found in the interval [",alpha1, ", ", alpha2,"]")
			zl <- list(alpha = NA, sigma = 1, mu = NA, m = Pos, s = Scal, ok = 0)
			return(zl)
		}

		alpha <- al$alpha
		sigma <- 1

		tmg <- .Fortran("rltrmng",
										alpha = as.double(alpha),
										sigma = as.double(sigma),
										beta = as.double(beta),
										mf = double(1),
                    PACKAGE = "robust")

		sigma <- Pos / tmg$mf
		mu <- alpha * sigma

		ok <- al$isol
    if(abs(Scal) <= 1e-6)
      ok <- 0

		zl <- list(alpha = alpha, sigma = sigma, mu = mu, m = Pos, s = Scal, ok= ok)

		u <- control$u
		Dq <- .Fortran("rlquqldg",
                   u = as.double(u),
                   alpha = as.double(alpha),
                   sigma = as.double(sigma),
                   tol = as.double(tol),
                   ql = double(1),
                   qu = double(1),
                   isol = integer(1),
                   PACKAGE = "robust")

		ok <- Dq$isol

		if(ok == 0) {
			Dq$ql <- NA
			mu <- NA
		}

		else {
			mnu <- x[x > Dq$ql & x <= Dq$qu]
      mu <- mean(mnu)
    }

		zl <- list(mu = mu, alpha = alpha, sigma = sigma, Tl = Dq$ql, Tu = Dq$qu,
               ok = ok)

		if(cov) {
			est <- 0
			tl <- 1e-10
			stl <- sigma*tl
			mu <- alpha*sigma
			lim <- 5*mu

			repeat {
				if(dgamma(lim / sigma, alpha) < stl) break
				lim <- lim + mu
			}

			xmax <- lim
			Theta <- S.Theta.gamma(alpha, sigma, u, beta, gam)
			one <- 1

			z <- .Fortran("rlquqldg",
										u = as.double(u),
										alpha = as.double(alpha),
										sigma = as.double(1),
										tol = as.double(tol),
										ql = double(1),
										qu = double(1),
										isol = integer(1),
                    PACKAGE = "robust")

			ok <- z$isol
			itc <- 0

			if(ok != 0 || itc != 0) {
				teta <- unlist(Theta)
				nt <- length(teta)
				til <- 1e-4
				z <- .Fortran("rlavtcmg",
											teta = as.double(teta),
											nt = as.integer(nt),
											alpha = as.double(alpha),
											sigma = as.double(sigma),
											itc = as.integer(itc),
											upper = as.double(xmax),
											til = as.double(til),
											sum = double(1),
											iwork = integer(80),
											work = double(320),
                      PACKAGE = "robust")

	     zl$V.mu <- z$sum / length(x)
			}
		}
	}

	else
		stop("Invalid Estimator")

	if(save.data)
		zl$data <- data

	zl$call <- the.call
	zl$header <- "Robust estimate of gamma distribution parameters"
	zl$plot.header <- "Robust Estimate of Gamma Density"
	zl$density.fn <- substitute(dgamma)
	zl$quantile.fn <- substitute(qgamma)
	oldClass(zl) <- c("gammaRob", "asmDstn")
	zl
}


