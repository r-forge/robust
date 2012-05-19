smooth.splineRob <- function(x, y, lambda = "RCp", lambda.range = c(0, 1),
		chuber = 1.345)
{
	the.call <- match.call()

##
## helper functions provided by Cantoni and Ronchetti
##

	e.psi <- function(chuber = 1.345) {

	# This function computes by numerical integration the expectations
	# terms appearing in the formulation of an M-type smoothing spline
	# and in the resistant criterion (RCV and RCp) for the selection
	# of the smoothing parameter.

		assign("chuber",chuber,frame=1)

		basepsi <- function(x)
			x*pmin(1,chuber/abs(x))

		basepsiprime <- function(x)
			1*(abs(x)<chuber)

		psiprime <- function(x)
			basepsiprime(x)*dnorm(x)

		psiprimecarre <- function(x)
			basepsiprime(x)*basepsiprime(x)*dnorm(x)

		psicarre <- function(x)
			basepsi(x)*basepsi(x)*dnorm(x)

		psiprimepsicarre <- function(x)
			basepsi(x)*basepsi(x)*basepsiprime(x)*dnorm(x)

		psicarrepsiprimecarre <- function(x)
			basepsiprime(x)*basepsiprime(x)*basepsi(x)*basepsi(x)*dnorm(x)

		psicarresurxcarre <- function(x)
			basepsi(x)*basepsi(x)/x^2*dnorm(x)

		psiquatsurxcarre <- function(x)
			basepsi(x)*basepsi(x)*basepsi(x)*basepsi(x)/x^2*dnorm(x)

		assign("basepsi", basepsi, frame = 1)
		assign("basepsiprime", basepsiprime, frame = 1)
		assign("psiprime", psiprime, frame = 1)
		assign("psiprimecarre", psiprimecarre, frame = 1)
		assign("psicarre", psicarre, frame = 1)
		assign("psiprimepsicarre", psiprimepsicarre, frame = 1)
		assign("psicarrepsiprimecarre", psicarrepsiprimecarre, frame = 1)
		assign("psicarresurxcarre", psicarresurxcarre, frame = 1)
		assign("psiquatsurxcarre", psiquatsurxcarre, frame = 1)

		epsiprime <- integrate(psiprime, lower = -Inf, upper = Inf)$integral
		epsiprimecarre <- integrate(psiprimecarre, lower = -Inf,
			upper = Inf)$integral
		epsicarre <- integrate(psicarre, lower = -Inf, upper = Inf)$integral
		epsiprimepsicarre <- integrate(psiprimepsicarre, lower = -Inf,
			upper = Inf)$integral
		epsicarrepsiprimecarre <- integrate(psicarrepsiprimecarre, lower = -Inf,
			upper = Inf)$integral
		epsicarresurxcarre <- integrate(psicarresurxcarre, lower = -Inf,
			upper = Inf)$integral
		epsiquatsurxcarre <- integrate(psiquatsurxcarre, lower = -Inf,
			upper = Inf)$integral

		list(	epsiprime = epsiprime,
					epsiprimecarre = epsiprimecarre,
					epsicarre = epsicarre,
					epsiprimepsicarre = epsiprimepsicarre,
					epsicarrepsiprimecarre = epsicarrepsiprimecarre,
					epsicarresurxcarre = epsicarresurxcarre,
					epsiquatsurxcarre = epsiquatsurxcarre)
  }


	frob <- function(xx, yy, lambda, chuber = 1.345, n.orig = length(xx),
						compteurmax = 500, mytol = .Machine$double.eps^.25, Smat = F)
	{

	# Fit of an M-type smoothing spline, as described in Eva Cantoni,
	# Elvezio Ronchetti, "Resistant Selection of the Smoothing Paramater
	# for Smooting Splines", Cahier du Département d'Econométrie,
	# University of Geneva, 98.06, August 1998

		if(length(xx) != length(yy)) 
			stop("lengths of x and y must match")

		if(missing(lambda))
			stop("smoothing parameter lambda not supplied")

	# Computation of the weights and of the vector of unique values of
	# "x" with the respective "y" values (cf. smooth.spline()) The
	# program works then with the sorted data (with respect to x)

		ww <- rep(1,length(xx))
		nb <- length(xx)
		sx <- unique(sort(xx))
		myo <- match(xx,sx)
		mynx <- length(sx)

		if(nb != mynx) {
			mycollaps <- .Fortran("suff",
														as.integer(nb),
														as.integer(mynx),
														as.integer(myo),
														as.double(xx),
														as.double(yy),
														as.double(ww),
														double(mynx),
														ybar = double(mynx),
														wbar = double(mynx),
														double(nb))

			sy <- mycollaps$ybar
			ww <- mycollaps$wbar
		}

		else {
			sy <- yy[order(xx)]
		}

	# Expectation terms for the RCp criterion

		esp.psi <- e.psi(chuber)
		epsiprime <- esp.psi$epsiprime
		epsicarre <- esp.psi$epsicarre
		epsicarresurxcarre <- esp.psi$epsicarresurxcarre
		epsiprimepsicarre <- esp.psi$epsiprimepsicarre
		epsiquatsurxcarre <- esp.psi$epsiquatsurxcarre
		epsiprimecarre <- esp.psi$epsiprimecarre
		epsicarrepsiprimecarre <- esp.psi$epsicarrepsiprimecarre

	# Initialisation

		lin.fit <- lsfit(sx,sy)
		frob.old <- sy-resid(lin.fit)
		sigmahat.old <- scale.tau(frob.old,weights=ww)
		myresid <- 1/sigmahat.old*resid(lin.fit)
		compteur <- 0
		coeff.spar <- (max(sx)-min(sx))^3

	# Iterative solution for an M-type smoothing spline

		repeat {
			vecpsi <- psihuber(myresid,chuber)
			ajust.new <- my.smooth.spline(x = sx,
										y = frob.old + sigmahat.old/epsiprime*vecpsi,
										w = ww,
										n.orig = n.orig,
										spar = lambda*sigmahat.old/epsiprime/coeff.spar/mean(ww),
										all.knots = F)

			frob.new <- ajust.new$y
			diag.matsol <- ajust.new$lev
			sigmahat.new <- scale.tau(y = sy,
												center = frob.new,
												weights = ww,
												init.scale = sigmahat.old)

			if(abs(max(frob.old-frob.new))/abs(max(myresid)) < mytol)
				break 

			myresid <- 1/sigmahat.new*(sy-frob.new)
			sigmahat.old <- sigmahat.new
			frob.old <- frob.new 

			compteur <- compteur + 1

			if(compteur > compteurmax)
				stop(paste("Convergence not obtained after",compteurmax,"iterations"))
		}

	# Cross-validation score. We suppose psi(0)=0.

		cv.score <- sigmahat.new^2 / epsiprime^2 * weighted.mean(
			(psihuber((yy-frob.new[myo]) / sigmahat.new, chuber) /
			(1-(diag.matsol[myo] * rep(1,length(yy))) / ww[myo]))^2,
			rep(1,length(yy)))
 
		sigma.ext <- NULL
		RCp <- NULL
		matsol <- NULL

		if(Smat) {
			Ident <- diag(1,nrow=length(sx))
			matsol <- matS(sx,lambda*sigmahat.old/epsiprime,wei=ww,n.orig)

		# External estimation of scale for the RCp criterion

			acoeff <- diff(sx)[-1]/diff(sx,lag=2)
			bcoeff <- diff(sx)[-(length(sx)-1)]/diff(sx,lag=2)
			normcoeff <- sqrt(ww[-c(length(sy)-1,length(sy))]^2*acoeff^2+
				ww[-c(1,2)]^2*bcoeff^2+ww[-c(1,length(sy))]^2)
			sigma.ext <- scale.tau((acoeff*ww[-c(length(sy)-1,length(sy))]*
				sy[-c(length(sy)-1,length(sy))]+bcoeff*ww[-c(1,2)]*sy[-c(1,2)]-
				ww[-c(1,length(sy))]*sy[-c(1,length(sy))])/normcoeff,center=0)

		# RCp criterion

			RCp.RASR <- sum(psihuber((yy-frob.new[myo])/sigma.ext,chuber)^2)
		  UmoinsV <- nb*epsicarre - 2/epsiprime*epsiprimepsicarre*sum(diag(matsol)) +
				1/sigma.ext^2*(epsiprimecarre - epsicarresurxcarre)*
				sum(diag((matsol-Ident)%*%frob.new%*%t(frob.new)%*%t(matsol-Ident)))+  
				1/epsiprime^2*(epsicarrepsiprimecarre - epsiquatsurxcarre)*
				sum(diag(matsol%*%t(matsol)))

			RCp <- RCp.RASR-UmoinsV
		}

	# Output

		output <- list(	estim = frob.new,
										sigmahat = sigmahat.new,
										sigma.ext = sigma.ext,
										x = sx,
										yin = sy,
										myo = myo,
										weights = ww,
										Smatrix = matsol,
										diagS = diag.matsol,
										epsiprime = epsiprime,
										epsicarre = epsicarre,
										cv.score = cv.score,
										RCp = RCp,
										chuber = chuber,
										lambda = lambda)

		invisible(output)
  }


	matS <- function(xx, lambda, wei, n.orig)
	{ 

	# Computation of the matrix defining a resistant spline.

		nb <- length(xx)
		matS <- matrix(ncol = nb, nrow = nb)
		Ident <- diag(1, nrow = nb)
		myspar <- lambda/(max(xx)-min(xx))^3/mean(wei)
   
		for(j in 1:nb) {
			matS[,j] <- my.smooth.spline(xx, Ident[,j], w = wei, n.orig = n.orig,
				spar = myspar, all.knots = F)$y 
		}

		matS
	}


	my.smooth.spline_function(x, y, w, n.orig, df = 5, spar = 0, cv = F,
		all.knots = F, df.offset = 0, penalty = 1)
	{

	# This function assumes that ties are treated before.

		my.call <- match.call()

		if(missing(y)) {
			if(is.list(x)) {
				if(anyMissing(match(c("x", "y"), names(x))))
					stop("cannot find x and y in list")
				y <- x$y
				x <- x$x
			}

			else if(is.complex(x)) {
				y <- Im(x)
				x <- Re(x)
			}

			else if(is.matrix(x) && ncol(x) == 2) {
				y <- x[, 2]
				x <- x[, 1]
			}

			else {
				y <- x
				x <- time(x)
			}
		}

		n <- length(x)

		if(n != length(y))
			stop("lengths of x and y must match.")

		if(missing(w))
			w <- rep(1, n)

		else {
			if(length(x) != length(w))
				stop("lengths of x and w must match.")

			if(any(w < 0))
				stop("all weights should be non-negative")

			if (sum(w)!=n.orig) stop("sum of weigths muste be equal to n.orig")
		}

		if(missing(spar))
			ispar <- 0

		else {
			if(spar < 1.01e-15)
				ispar <- 0

			else
				ispar <- 1
		}

		if(cv) 
			icrit <- 2

		else 
			icrit <- 1
	
		dfinfo <- df.offset

		if(!missing(df)) {
			if(df > 1 & df < n) {
				icrit <- 3
				dfinfo <- df
			}

			else
				warning("you must supply 1 < df < n")
		}

		n <- as.integer(n)
		isetup <- as.integer(0)
		ier <- as.integer(1)
		mode(x) <- "double"
		mode(y) <- "double"
		mode(w) <- "double"
		mode(spar) <- "double"
		mode(ispar) <- "integer"
		mode(icrit) <- "integer"
		mode(isetup) <- "integer"
		mode(ier) <- "integer"
		sx <- unique(sort(x))
		o <- match(x, sx)
		nx <- length(sx)

		xbar <- (sx - sx[1])/(sx[nx] - sx[1])

		if(all.knots) {
			knot <- c(rep(xbar[1], 3), xbar, rep(xbar[nx], 3))
			nk <- nx + 2
		}

		else {
			knot <- .Fortran("sknotl",
												xbar,
												as.integer(nx),
												knot = double(n + 6),
												k = integer(1))

			nk <- knot$k - 4
			knot <- knot$knot[seq(knot$k)]
		}

		low.parm <- 0
		high.parm <- 1.5

	fit <- .Fortran("qsbart",
									as.double(penalty),
									as.double(dfinfo),
									x = xbar,
									y = y, # y = collaps$ybar
									w = w, # w = collaps$wbar
									as.integer(nx),
									knot,
									as.integer(nk),
									coef = double(nk),
									ty = double(nx),
									lev = double(nx),
									crit = double(1),
									c(icrit, ispar),
									spar = spar,
									c(as.double(low.parm),
									as.double(high.parm),
									as.double(0.001)),
									isetup,
									double((17 + nk) * nk),
									as.integer(4),
									as.integer(1),
									ier = ier)

		if(fit$ier > 0) {
			warning("smoothing parameter value too small or too large")
			fit$ty <- rep(mean(y), nx)
		}

		lev <- fit$lev
		df <- sum(lev)
		win <- w[1:nx] # win <- collaps$wbar[1:nx]

		if(cv) {
			ww <- win
			ww[!(ww > 0)] <- 1	# for the next line
			cv.crit <- weighted.mean(((y - fit$ty[o])/(1 - (lev[o] * w)/
									ww[o]))^2, w)
		}

		else
			cv.crit <- weighted.mean((y - fit$ty[o])^2, w)/(1 - (df.offset + 
				penalty * df)/sum(win))^2

		pen.crit <- sum(win * (y - fit$ty) * y)
		fit.object <- list(knot = knot, nk = nk, min = sx[1],
			range = sx[nx] - sx[1], coef = fit$coef)
		class(fit.object) <- "smooth.spline.fit"

		object <- list(	x = sx,
										y = fit$ty,
										w = win,
										yin = y[1:nx], 
										lev = lev,
										cv.crit = cv.crit,
										pen.crit = pen.crit,
										df = df, 
										spar = fit$spar,
										fit = fit.object,
										call = my.call)

		class(object) <- "smooth.spline"
		object
	}


	opt.RCp <- function(xx, yy, bornes.opt, ind.chuber = 1.345)
	{

	# Fit of an M-type smoothing spline with parameter chosen by
	# robust C_p, as described in Eva Cantoni, Elvezio Ronchetti,
	# "Resistant Selection of the Smoothing Paramater for Smooting
	# Splines", Cahier du Département d'Econométrie, University of
	# Geneva, 98.06, August 1998

		if(length(xx) != length(yy)) 
			stop("lengths of x and y must match.")

		nb_length(xx)

	# Expectation terms for the RCp criterion

		psi.integr <- e.psi(ind.chuber)
		epsiprime <- psi.integr$epsiprime
		epsicarre <- psi.integr$epsicarre
		epsicarresurxcarre <- psi.integr$epsicarresurxcarre
		epsiprimepsicarre <- psi.integr$epsiprimepsicarre
		epsiquatsurxcarre <- psi.integr$epsiquatsurxcarre
		epsiprimecarre <- psi.integr$epsiprimecarre
		epsicarrepsiprimecarre <- psi.integr$epsicarrepsiprimecarre
		assign("ind.chuber",ind.chuber,frame=1)
		assign("bornes.opt",bornes.opt,frame=1)

		sx <- unique(sort(xx))
		Ident <- diag(length(sx))
		assign("Ident",Ident,frame=1)
		myo <- match(xx,sx)

	# Objective function to minimize.

		RCp.value <- function(lambda, xx, yy, nb, epsiprime, epsicarre,
									epsiprimepsicarre, epsiprimecarre, epsicarresurxcarre,
									epsicarrepsiprimecarre, epsiquatsurxcarre, myo)
		{
			ajust <- frob(xx,yy,lambda,chuber=ind.chuber,Smat=T)
			fchap <- ajust$estim
			Smatrix <- ajust$Smatrix
			sigma.ext <- ajust$sigma.ext
			ww <- ajust$weights
			RCp.RASR <- sum(psihuber((yy-fchap[myo])/sigma.ext,ind.chuber)^2)
			UmoinsV <- nb*epsicarre - 2/epsiprime*epsiprimepsicarre*sum(diag(Smatrix))+ 
				1/sigma.ext^2*(epsiprimecarre - epsicarresurxcarre)*
				sum(diag((Smatrix-Ident)%*%fchap%*%t(fchap)%*%t(Smatrix-Ident)))+ 
				1/epsiprime^2*(epsicarrepsiprimecarre - epsiquatsurxcarre)*
				sum(diag(Smatrix%*%t(Smatrix)))
			RCp.RASR-UmoinsV
		}

	# Minimisation

		opt.lambda <- optimize(RCp.value, bornes.opt, xx = xx, yy = yy, nb = nb,
			epsiprime=epsiprime, epsicarre = epsicarre,
			epsiprimepsicarre = epsiprimepsicarre,
			epsiprimecarre = epsiprimecarre,
			epsicarresurxcarre = epsicarresurxcarre,
			epsicarrepsiprimecarre = epsicarrepsiprimecarre,
			epsiquatsurxcarre = epsiquatsurxcarre,
			myo = myo)

	# Optimal values.

		lambda.opt <- opt.lambda$minimum
		RCp.opt <- opt.lambda$objective

	# Fit for the optimal value of the smoothing parameter

		ajust.opt <- frob(xx,yy,lambda.opt,chuber=ind.chuber,Smat=T)

	# Output

		output <- list(	lambda.opt = lambda.opt,
										RCp.opt = RCp.opt,
										ajust.opt = ajust.opt)

		invisible(output)
	}


	opt.cv <- function(xx, yy, bornes.opt, ind.chuber = 1.345)
	{

	# Fit of an M-type smoothing spline with parameter chosen by
	# robust cross-validation, as described in Eva Cantoni, Elvezio 
	# Ronchetti, "Resistant Selection of the Smoothing Paramater
	# for Smooting Splines", Cahier du Département d'Econométrie,
	# University of Geneva, 98.06, August 1998

		if(length(xx) != length(yy)) 
			stop("lengths of x and y must match.")

		nb <- length(xx)
		psi.integr <- e.psi(ind.chuber)
		epsiprime <- psi.integr$epsiprime
		assign("ind.chuber",ind.chuber,frame=1)
		assign("bornes.opt",bornes.opt,frame=1)

	# Distinct values of xx. 

		sx <- unique(sort(xx))
		myo <- match(xx,sx)

	# Objective function to minimize.

		cv.value <- function(lambda,xx,yy,nb,epsiprime,myo) {
			ajust <- frob(xx,yy,lambda,chuber=ind.chuber)
			fchap <- ajust$estim
			diagS <- ajust$diagS
			sigmahat <- ajust$sigmahat
			ww <- ajust$weights
			sigmahat^2/epsiprime^2*weighted.mean((psihuber((yy-fchap[myo])/sigmahat,
				ind.chuber)/(1-(diagS[myo]*rep(1,length(yy)))/ww[myo]))^2,
				rep(1,length(yy)))
		}

	# Minimisation

		opt.lambda <- optimize(cv.value, bornes.opt, xx = xx, yy = yy, nb = nb,
			epsiprime = epsiprime, myo = myo)

	# Optimal values.

		lambda.opt <- opt.lambda$minimum
		cv.opt <- opt.lambda$objective

	# Fit for the optimal value of the smoothing parameter

		ajust.opt <- frob(xx,yy,lambda.opt,chuber=ind.chuber)

	# Output

		output <- list(	lambda.opt = lambda.opt,
										cv.opt = cv.opt,
										ajust.opt = ajust.opt)

		invisible(output)
	}


	psihuber <- function(x, chuber)
  {

	# This function defines the psi Huber function, 
	# with tuning constant equal to chuber.

  	x*pmin(1,chuber/abs(x))
  }

	assign("e.psi", e.psi, frame = 1)
	assign("frob", frob, frame = 1)
	assign("matS", matS, frame = 1)
	assign("my.smooth.spline", my.smooth.spline, frame = 1)
	assign("opt.RCp", opt.RCp, frame = 1)
	assign("opt.cv", opt.cv, frame = 1)
	assign("psihuber", psihuber, frame = 1)

	if(lambda == "cv")
		z <- opt.cv(x, y, bornes.opt = lambda.range,
									ind.chuber = chuber)$ajust.opt

	else if(lambda == "RCp")
		z <- opt.RCp(x, y, bornes.opt = lambda.range,
									ind.chuber = chuber)$ajust.opt

	else
		z <- frob(x, y, lambda, chuber = chuber)

	z$call <- the.call
	oldClass(z) <- "smooth.splineRob"
	z
}


print.smooth.splineRob <- function(x, ...)
{
	if(!is.null(x$call)) {
		cat("Call:\n")
		dput(x$call)
	}

	cat("\nSmoothing Parameter (lambda):", format(x$lambda), "\n")

	invisible(x)
}


lines.smooth.splineRob <- function(object, ...)
{
	lines(object$x, object$estim, ...)
	invisible()
}

