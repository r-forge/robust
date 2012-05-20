glmRob.cubif.null <- function(x, y, ni, offset, ics, family, control,
  robust.cov.control)
{
  tua <- epsilon <- control$epsilon
  mxf <- mxt <- maxit <- control$maxit
  trc <- control$trc
  gma <- 1
  iug <- 1
  ipo <- 1
  ilg <- 1
  icn <- 1
  icv <- 1
  ia1 <- 1
  rank <- 1

  bpar <- control$bpar
  cpar <- control$cpar

  n <- ly <- length(y)
  w <- rep(1,ly)
  ai <- ci <- rep(0, ly)
  intl <- attr(x, "term.labels")

  if(is.null(intl)) 
    int <- FALSE
  else 
    int <- as.logical(match(intl[1], c("(Int.)", "(Intercept)"), FALSE))

  if(!int) {
    eta <- rep(0,ly)
#    mu <- family$inverse(eta+offset) 
    mu <- family$linkinv(eta+offset) 
    cval <- 0.5

    if(ics == 3)
      cval <- 1

    ci <- rep(cval, ly)
    ai <- rep(9999., ly) 
  }

  else {
    X <- as.matrix(x[,1])

  zi <- glmRob.cubif.Ini(X = X, y = y, ni = ni, icase = ics,
    offset = offset, b = bpar, zmin = 0.001, epsilon = epsilon,
    ia1 = ia1, control = robust.cov.control)

  ci <- zi$ci
  cov <- zi$cov
  ai <- zi$ai
  theta <- theta0 <- zi$theta

  p <- 1

  sqcov  <- rep(1.0, p)

  for(i in 1:p) {
    j <- (i*(i+1))/2; 
    sqcov[i] <- sqrt(cov[j])
  } 

  # Iterations #

    nit <- 1
    repeat {

  # theta-step #

      zt <- glmRob.gytstp(X, y, ci, theta, ai, cov,
        ni, offset, tol = epsilon, icase = ics, maxit = mxt)

      theta  <- zt$theta[1:p]
      vtheta <- zt$vtheta
      nitt <- zt$nit

  # Check convergence #

      if(nit == maxit)
        break

      delta <- theta-theta0

      if(all(epsilon*sqcov-abs(delta) > 0.0))
        break

      theta0 <- theta

  # c-step #

      zc <- glmRob.gicstp(icase = ics, ialg = ilg, ni = ni,
        vtheta = vtheta, ai = ai, oi = offset, tol = epsilon,
        maxit = mxt)

      ci <- zc$ci
      nit <- nit+1
    }

  # end of iterations #

  # Final covariance matrix of estimated coefficients
    z <- glmRob.gfedca(vtheta,ci,ai,ni,offset,icase=ics)
    zc <- glmRob.ktaskw(x = X, d = z$d, e = z$e, f = 1/n, f1 = 1, iainv = 0,
      ia = ia1, tau = epsilon)
    covf <- zc$cov

    zf <- list(theta = theta, ai = ai, vtheta = vtheta, ci = ci,
      cov = covf, nit = nit, delta = delta)

#          z    <- glmRob.gintac(X, y, ni, offset, icase = ics, 
#         tolt=10*epsilon,
#         tola=10*epsilon, b = upar, c = cpar, maxtt=mxt, 
#         maxta=mxf)
#          t0   <- z$theta[1] 
#       A0 <- z$a
#       c0 <- z$ci
#          wa   <- upar/pmax(1.e-4,z$dist)
#          vtheta <- rep(t0,ly)
#          z    <- glmRob.gfedca(vtheta, c0, wa, ni, offset, ics)
#          zc   <- glmRob.ktaskw(X, z$d, z$e, f=1/ly, f1=1, iainv=0,
#       ia = ia1, tau = epsilon)
#          covi <- zc$cov
#          if (icn != 1) covi <- 1/covi
#          zf   <- glmRob.gymain(X, y, ni, covi, A0, t0, offset, 
#       b=upar, gam=gma, 
#       tau=epsilon, icase=ics, iugl=iug, iopt=ipo, 
#       ialg=ilg, icnvt=icn,
#       icnva=icv, maxit=maxit, maxtt=mxt, maxta=mxf, 
#       maxtc=mxt, 
#       tol=epsilon, tolt=10*epsilon, tola=10*epsilon, tolc=10*epsilon,
#       trc=trc)
#           ai   <- zf$wa
#           ci   <- zf$ci

    eta <- zf$vtheta
#    mu <- family$inverse(eta+offset)
    mu <- family$linkinv(eta+offset)
  }

#  family$deviance(mu, y/ni, w)
  sum(family$dev.resids(y/ni, mu, w))
}


