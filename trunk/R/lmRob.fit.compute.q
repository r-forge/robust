lmRob.fit.compute <- function(x2, y, x1=NULL, x1.idx=NULL, nrep=NULL,
                       robust.control=NULL, genetic.control=NULL, ...)
{

##
## Step 0. Set Control Parameters
##

  tua <- robust.control$tua
  tlo <- robust.control$tlo
  mxr <- robust.control$mxr
  mxf <- robust.control$mxf
  mxs <- robust.control$mxs
  tl  <- robust.control$tl
  psi <- robust.control$weight
  sed <- robust.control$seed
  ini <- robust.control$initial.alg
  fnl <- robust.control$final.alg
  est <- robust.control$estim
  eff <- robust.control$efficiency
  trc <- robust.control$trace

  if(is.null(x1)) {
    x <- as.matrix(x2)
    xnames <- dimnames(x)[[2]]
    n <- nrow(x)
    p <- p2 <- ncol(x)
  }

  else {
    x1 <- as.matrix(x1)
    p1 <- ncol(x1)

    if(is.null(x1.idx))
      x1.idx <- 1:p1

    if (!is.null(x2)) {
      x2 <- as.matrix(x2)
      p2 <- ncol(x2)
    }

    else
      p2 <- 0

    p <- p1+p2
    n <- nrow(x1)
    x <- matrix(NA, nrow=n, ncol=p)
    xnames <- rep(NA, p)
    x[, x1.idx] <- x1
    xnames[x1.idx] <- dimnames(x1)[[2]]

    if (p2) {
      x[,-x1.idx] <- x2
      xnames[-x1.idx] <- dimnames(x2)[[2]]
    }

  }

  if(casefold(ini) == "auto") {
    if((n <= 250 && p == 2) || (n <= 80 && p == 3)) 
      ini <- "exhaustive"
    else if (p2 > 15)
      ini <- "fast"
    else 
      ini <- "random"
  }

  if(casefold(ini) == "random") {
    iop <- 2

    if(is.null(nrep)) 
      nrep <- round(4.6*2^p2)

    if (p2 > 15 && trc) {
      if(exists("guiDisplayDialog")) {
        guiDisplayDialog("Function", "confirmLmRob", bModal=TRUE)
        if (yes.p == FALSE)
          return(NULL)
      }

      else {
        yesorno <- c("Yes", "No")
        title <- paste("Random resampling may take a long time.",
                       "Do you want to continue?")
        wmenu <- menu(yesorno, title=title)
        if (wmenu != 1) 
          return(NULL)
      }
    }
  }

  else if(casefold(ini) == "exhaustive") {

    if (n > 300 || p2 > 10)
      stop("The data set is large. ",
           "Try using random resampling or the fast procedure.")

    iop <- 3
    nrep <- choose(n,p2)
  }

  else if(casefold(ini) == "genetic")
    iop <- 4

  else if(casefold(ini) == "fast")
    iop <- 6

  else
    stop("Invalid choice of resampling method.")

  if (p2 == 0)
    iop <- 5

  msg.UCV <- paste("Sum(psi.weight(wi)) less than",tl,"in lmRob.ucovcoef.")

  if(match("(Intercept)", xnames, nomatch = 0) == 0)
    intercept <- FALSE

  else 
    intercept <- TRUE

  if(length(y) != n)
    stop(paste("Length of response vector must be the same", 
         " as the number of rows in the independent variables",sep=""))

  qrx <- qr(x)[c("rank", "pivot")]
  rank <- qrx$rank
  if(rank < p) stop(paste("x is singular, rank(x)=", rank))

##
## Step 1. Compute Initial Estimates
##

  if(casefold(psi[1]) == "bisquare") {
    ipsi <- 2
		xk <- 1.5477
		beta <- 0.5
  }

  else if(casefold(psi[1]) == "optimal") {
    ipsi <- 1
		xk <- 0.4047
		beta <- 0.2661
  }

  else
    stop("Invalid choice of weight function.")

  bet0 <- 0.773372647623 #bet0 = pnorm(0.75)
	
  if(iop == 5) {
    tmp <- lmRob.lar(x, y, tol=tl)
    coeff0 <- tmp$coef
    resid0 <- tmp$resid
    scale0 <- mad(resid0)
    tmpn <- double(n)
    z1 <- .Fortran("rlrsigm2",
                   as.double(resid0),
                   as.double(resid0),
                   SIGMAI=as.double(scale0),
                   as.integer(n),
                   as.integer(p),
                   as.double(tlo),
                   ITYPE=as.integer(1),
                   ISIGMA=as.integer(1),
                   MAXIS=as.integer(mxs),
                   NIT=integer(1),
                   SIGMAF=double(1),
                   SW=tmpn,
                   SC=tmpn,
                   as.integer(ipsi),
                   as.double(xk),
                   as.double(beta),
                   as.double(bet0),
                   PACKAGE = "robust")
    scale0 <- z1$SIGMAF
  }

  else if(iop == 4) { 
#    set.seed(sed)
#
#    if(is.null(genetic.control))
#      genetic.control <- lmRob.genetic.control()
#
#    z1 <- lmRob.ga(x, y, rank, genetic.control, ipsi, xk, beta, tolr=tlo,
#                   tau=tua, maxs1=mxs)
#
#    coeff0 <- z1$theta[1:rank]
#    scale0 <- z1$smin
#    resid0 <- z1$rs
  }

  else if(!is.null(x1)) {
    tmp <- lmRob.lar(x1, y, tol=tl)
    y.tilde <- tmp$resid
    t1 <- tmp$coef
    x2.tilde <- x2
    T2 <- matrix(0, nrow=p1, ncol=p2)
		
##	linux differs from everything else at least here

    for(i in 1:p2) {
      tmp <- lmRob.lar(x1, x2[,i], tol=tl)
      x2.tilde[,i] <- tmp$resid
      T2[,i] <- tmp$coef
    }

    tmpn <- double(n)
    tmp1 <- double(p1)
    tmpp <- double(p2)
    SFGH1 <- matrix(double(1), nrow=p1, ncol=3)
    SFGH2 <- matrix(double(1), nrow=p2, ncol=3)
    xx <- matrix(double(1),p2,p2)
    storage.mode(x1) <- storage.mode(x2.tilde) <- "double"
    z1 <- .Fortran("rlfrstml",
                   X1=x1,
                   X2=x2.tilde,
                   Y=as.double(y.tilde),
                   as.integer(n),
                   as.integer(p1),
                   as.integer(p2),
                   as.integer(p2),
                   as.integer(n),
                   T1=tmp1,
                   T2=tmpp,
                   x1c=x1,
                   XTHETA1=tmpn,
                   XTHETA2=tmpp,
                   XX=xx,
                   YY=tmpp,
                   iopt=as.integer(iop),
                   INTCH=as.integer(1),
                   as.integer(nrep),
                   TOLS=as.double(tlo),
                   TOLR=as.double(tlo),
                   TAU=as.double(tua),
                   MAXS1=as.integer(mxs),
                   as.integer(sed),
                   IERR=integer(1),
                   SMIN=double(1),
                   tmpn,
                   tmpn,
                   SFGH1,
                   SFGH2,
                   integer(p2),
                   tmpn,
                   integer(p2),
                   as.integer(ipsi),
                   as.double(xk),
                   as.double(beta),
                   as.double(bet0),
                   as.integer(trc),
                   PACKAGE = "robust")
    b2 <- z1$T2
    b1 <- t1 + z1$T1 -T2 %*% b2
    coeff0 <- rep(NA, p)
    coeff0[x1.idx]  <- b1
    coeff0[-x1.idx] <- b2
    scale0 <- z1$SMIN
    resid0 <- y - x1 %*% b1 - x2 %*% b2
  }

  else if(iop == 6) {
    storage.mode(x) = "double"
    storage.mode(y) = "double"
    tmpn = double(n)
    tmpp = double(p)
    tmpi = integer(p)
    tmpm = matrix(double(1), p, p)
    n = as.integer(n)
    p = as.integer(p)
    z1 <- .Fortran("rlfastse",
                   x,
                   x,
                   n,
                   p,
                   y,
                   y,
                   n,
                   RES=tmpn,
                   TAU=as.double(tua),
                   K=integer(1),
                   SF=tmpp,
                   SG=tmpp,
                   SH=tmpp,
                   IP=tmpi,
                   XPXH=tmpm,
                   XPXI=tmpm,
                   HDIAG=tmpn,
                   Q=tmpm,
                   U=tmpm,
                   Z=x,
                   ITER=integer(1),
                   IERR=integer(1),
                   x,
                   y,
                   SMIN=as.double(0),
                   as.integer(ipsi),
                   as.double(xk),
                   as.double(beta),
                   as.double(bet0),
                   as.integer(mxs),
                   as.double(tlo),
                   as.double(tlo),
                   as.integer(mxs),
                   THETA=tmpn,
                   XTHETA1=tmpp,
                   XTHETA2=tmpp,
                   IPERM=integer(n),
                   C1=as.double(2),
                   PACKAGE = "robust")
    if(z1$IERR == 1) 
      stop("Singular matrix encountered.")
    coeff0 = z1$XTHETA2
    scale0 = z1$SMIN
    resid0 = y - x %*% coeff0
  }

  else {
    mdi <- p + rank
    mdw <- (p+2)*rank + 3*p + n
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    tmpn <- double(n)		
    z1 <- .Fortran("rlhsesm2",
                   x,
                   y,
                   as.integer(n),
                   as.integer(p),
                   nq=as.integer(rank),
                   mdx=as.integer(n),
                   as.integer(mdw),
                   as.integer(mdi),
                   iopt=as.integer(iop),
                   intch=as.integer(1),
                   as.integer(nrep),
                   tols=as.double(tlo),
                   tolr=as.double(tlo),
                   tau=as.double(tua),
                   maxs1=as.integer(mxs),
                   iseed=as.integer(sed),
                   ierr=integer(1),
                   smin=double(1),
                   theta=tmpn,
                   rs=tmpn,
                   it1=integer(rank),
                   work=double(mdw),
                   iwork=integer(mdi),
                   as.integer(ipsi),
                   as.double(xk),
                   as.double(beta),
                   as.double(bet0),
                   as.integer(trc),
                   PACKAGE = "robust")
    coeff0 <- z1$theta[1:rank]
    scale0 <- z1$smin
    resid0 <- z1$rs
  }
	
  if(scale0 < tl) 
    warning(paste("Initial scale less than tl = ", tl, "."))

  z1 <- lmRob.ucovcoef(x, y, resid0, scale0, rank, ipsi=ipsi, xk=xk,
                       tau=tua, tl=tl) 
  if(z1$ierr==1)
    warning(paste(msg.UCV," during initial estimation.")) 

  ucov0 <- z1$ucov
  M.weights0 <- z1$wi

##
## Step 2. Refine Initial Estimates
##

  if(iop == 5) {
    tmpp <- double(p)
    storage.mode(x) <- "double"
    z1 <- .Fortran("rlywagm2",
                   x,
                   as.double(y),
                   theta=as.double(coeff0),
                   sigmaf=as.double(scale0),
                   as.integer(n),
                   as.integer(p),
                   as.integer(n),
                   as.double(tlo),
                   GAM=as.double(1),
                   as.double(tua),
                   as.integer(mxr),
                   nit=integer(1),
                   rs=as.double(resid0),
                   tmpp,
                   tmpn,
                   tmpp,
                   tmpp,
                   tmpp,
                   integer(p),
                   x,
                   PACKAGE = "robust")
  }

  else if(is.null(x1)) {
    coeffi <- vector("numeric", max(n, rank))
    coeffi[1:rank] <- coeff0
    z1 <- lmRob.wm(x, y, coeffi, ucov0, scale0, ipsi=ipsi, xk=xk,
                   beta=beta, tlo=tlo, tua=tua, mxr=mxr)
  }

  else { 
    A <- matrix(double(1), p1, p2)
    B <- matrix(double(1), p1, p1)
    storage.mode(x1) <- storage.mode(x2) <- "double"
    z1 <- .Fortran("rldscnm2",
                  x1,
                  x2,
                  as.double(y),
                  as.integer(n),
                  as.integer(p1),
                  as.integer(p2),
                  as.integer(n),
                  as.double(scale0),
                  sigmaf=double(1),
                  as.double(b1),
                  as.double(b2),
                  b1=tmp1,
                  b2=tmpp,
                  rs=as.double(resid0),
                  RSTMP=tmpn,
                  as.double(tlo),
                  as.double(tua),
                  as.integer(mxr),
                  as.integer(mxs),
                  SFGH1,
                  as.integer(ipsi),
                  as.double(xk),
                  as.double(beta),
                  double(1),
                  IFAIL=integer(1),
                  tmpn,
                  A,
                  B,
                  CC=xx,
                  C2=xx,
                  D=x2,
                  BD=A,
                  H=tmpp,
                  tc=as.double(1.55),
                  x1,
                  IP=integer(p1),
                  IDX=integer(p2),
                  WP1=tmp1,
                  WP2=tmpp,
                  nit=integer(1),
                  MAXK=as.integer(mxr),
                  PACKAGE = "robust")
    z1$theta <- rep(NA, p)
    z1$theta[ x1.idx] <- z1$b1
    z1$theta[-x1.idx] <- z1$b2
  }

  iter.refinement <- z1$nit
  tmp <- z1$sigmaf

  if(tmp < tl) 
    warning(paste("Refined scale less than tl = ", tl, "."))

  if(iter.refinement == mxr) 
    warning("Max iteration for refinement reached.")

  if(tmp < scale0) {
    coeff0 <- z1$theta[1:rank]
    scale0 <- tmp
    resid0 <- z1$rs

    z1 <- lmRob.ucovcoef(x, y, resid0, scale0, rank, ipsi=ipsi, xk=xk,
						tau=tua,tl=tl)

    if(z1$ierr==1)
      warning(paste(msg.UCV," when refining initial estimates.")) 

    ucov0 <- z1$ucov
    M.weights0 <- z1$wi
  }

  names(coeff0) <- xnames
  attributes(resid0) <- attributes(y)

##
## Step 3. Robust Cov, Dev, and R2 of Initial Estimates
##

  dev0 <- scale0^2
  tmp <- ucov0 * dev0 / n
  scov0 <- matrix(NA, nrow = rank, ncol = rank)
  dimnames(scov0) <- list(xnames, xnames)
  i2 <- 0

  for(i in 1:rank) { 
    i1 <- i2 + 1
    i2 <- i1 + i - 1
    scov0[i, 1:i] <- tmp[i1:i2]
    scov0[1:i, i] <- tmp[i1:i2]
  }

  t0 <- if (intercept) median(y) else 0
  ymt <- abs(y - t0)
  s0 <- median(ymt)/0.6745

  if(s0 == 0) {
    s0 <- min(ymt[ymt != 0])
    while(sum(chi.weight(ymt/s0,ipsi,xk)) > (n - 1) * 0.5) 
      s0 <- 1.5 * s0
  }

  tmp <- matrix(1, ncol = 1, nrow = n)
  ymt[1] <- t0

  z1 <- lmRob.wm(tmp, y, ymt, ucov0[1], s0, isigma=1, ipsi=ipsi,
                 xk=xk, beta=beta, tlo=tlo, tua=tua, mxr=mxr)

  tstar <- if (intercept) z1$theta[1] else 0
  sy <- (z1$sigmaf)^2
  r.squared0 <- ((n-intercept)*sy - (n-rank)*scale0^2)/((n-intercept)*sy)
  fval0 <- y-resid0
  dfres <- length(resid0) - rank

  z <- list(coefficients=coeff0, scale=scale0, residuals=resid0,
            fitted.values=fval0, cov=scov0, rank=rank,
            iter.refinement=iter.refinement,  df.residual=dfres, 
            est="initial", robust.control=robust.control)

##
## Step 4. Compute Final Estimates
##

  if (casefold(est) == "final") {
    coeffi <- vector("numeric", max(n, rank))
    coeffi[1:rank] <- coeff0

    if (casefold(fnl) == "mm" || casefold(fnl) == "m") {
      if (casefold(psi[2]) == "optimal") {
        ipsi2 <- 1
        if (eff == 0.95)      yc <- 1.060158
        else if (eff == 0.9)  yc <- 0.9440982
        else if (eff == 0.85) yc <- 0.8684
        else if (eff == 0.8)  yc <- 0.8097795
        else                  yc <- lmRob.effvy(eff)
      }

      else if (casefold(psi[2]) == "bisquare") {
        ipsi2 <- 2
        if (eff == 0.95)      yc <- 4.685061
        else if (eff == 0.9)  yc <- 3.882646
        else if (eff == 0.85) yc <- 3.443689
        else if (eff == 0.8)  yc <- 3.136909
        #else                  yc <- chb(eff)$cb
        else                  yc <- lmRob.effvy(eff, ipsi = 2)
      }

      else 
        stop("Invalid choice of weight function.")

      z1 <- lmRob.wm(x, y, coeffi, ucov0, scale0, isigma=0, ipsi=ipsi2, 
                    xk=yc, beta=beta, tlo=tlo, tua=tua, mxr=mxf)

      iter.final.coef <- z1$nit

      if(iter.final.coef == mxf) 
        warning("Max iteration for final coefficients reached.")

      ymt <- vector("numeric", n)
      ymt[1] <- tstar
      tmp <- matrix(1, nrow = n, ncol = 1)

      if(intercept) {
        that <- lmRob.wm(tmp, y, ymt, ucov0[1], scale0, isigma=0, 
                         ipsi=ipsi2, xk=yc, beta=beta, tlo=tlo, 
                         tua=tua, mxr=mxf)$theta[1]
      }

      else 
        that <- 0

      ymt <- (y - that)/scale0
      sy <- sum(chi.weight(ymt,ipsi2,yc))
      ymt <- z1$rs/scale0
      s0 <- sum(chi.weight(ymt,ipsi2,yc))
      r.squared1 <- (sy - s0)/sy
      dev1 <- (scale0^2) * s0

      z2 <- lmRob.ucovcoef(x, y, resid0, scale0, rank, ipsi=ipsi2,
                           xk=yc, tau=tua, tl=tl)

      if(z2$ierr == 1) 
        warning(paste(msg.UCV,"during final scale estimation.")) 

      ucov1 <- z2$ucov
      M.weights1 <- z2$wi
      tmp <- ucov1 * scale0^2 / n
      scov1  <- matrix(NA, nrow = rank, ncol = rank)
      dimnames(scov1) <- list(xnames,xnames)
      i2 <- 0

      for(i in 1:rank) {
        i1 <- i2 + 1
        i2 <- i1 + i - 1
        scov1[i, 1:i] <- tmp[i1:i2]
        scov1[1:i, i] <- tmp[i1:i2]
      }

      z2 <- .Fortran("rlrsigm2",
                     z1$rs,
                     wgt=y,
                     sigmai=as.double(2*scale0),
                     as.integer(n),
                     np=as.integer(rank),
                     tol=as.double(tlo),
                     itype=as.integer(1),
                     isigma=as.integer(1),
                     maxis=as.integer(mxs),
                     nit=integer(1),
                     sigmaf=double(1),
                     double(n),
                     double(n),
                     as.integer(ipsi),
                     as.double(xk),
                     as.double(beta),
                     as.double(1.0),
                     PACKAGE = "robust")
      scale1 <- z2$sigmaf
    }

    else if (casefold(fnl) == "adaptive") {
      tmp <- double(p)
      residi <- resid0
      storage.mode(x) <- storage.mode(y) <- "double"
      eta <- lmRob.effad(eff)
      nit <- 1

      for (i in 1:nit) {
        z1 <- .Fortran("rlfinlml",
                       X=x,
                       Y=y,
                       WGT=double(n),
                       rs=as.double(residi),
                       as.integer(n),
                       as.integer(p),
                       as.integer(n),
                       theta=as.double(coeffi),
                       as.double(scale0),
                       tmp,
                       tmp,
                       tmp,
                       integer(p),
                       x,
                       y,
                       as.double(tua),
                       as.double(eta),
                       ierr=integer(1),
                       as.integer(ipsi),
                       as.double(xk),
                       f=double(1),
                       double(n),
                       PACKAGE = "robust")
        coeffi <- z1$theta
        residi <- z1$rs
      }

      if (z1$ierr == 1) {
        tmp <- paste("The weighted model matrix is singular.",
                     "Only initial estimates are returned.")
        warning(tmp)
        est <- "initial"
      }

      scov1 <- scale0*scale0*solve(t(x) %*% x)*z1$f
      dimnames(scov1) <- list(xnames, xnames)
    }

    else
      stop("Invalid choice of final estimation.")

    coeff1 <- z1$theta[1:rank]
    names(coeff1) <- xnames
    resid1 <- z1$rs
    attributes(resid1) <- attributes(y)
    fval1 <- y - resid1
  }

  if (casefold(est) == "final") {

    z <- list(coefficients=coeff1, scale=scale0, residuals=resid1,
              fitted.values=fval1, cov=scov1, rank=rank,
              iter.refinement=iter.refinement, df.residual=dfres,
              est="final", robust.control=robust.control)

    z$T.coefficients  <- coeff0
    z$T.residuals     <- resid0
    z$T.fitted.values <- fval0
    z$T.cov          <- scov0
    if (fnl == "MM" || fnl == "m" || fnl == "M") {
      z$dev <- dev1;              z$T.dev <- dev0
      z$M.weights <- M.weights1;  z$T.M.weights <- M.weights0
      z$r.squared <- r.squared1;  z$T.r.squared <- r.squared0
      z$T.scale         <- scale1
      z$iter.final.coef <- iter.final.coef
    }
  }

  else {
    if(is.null(x1)) {
      z$dev <- dev0
      z$M.weights <- M.weights0
      z$r.squared <- r.squared0
    }
  } 

  if(!is.null(genetic.control))
    z$genetic.control <- genetic.control 
  z$qr <- qrx
	z$yc <- lmRob.effvy(eff)
  oldClass(z) <- c("lmRob")
	z
}


