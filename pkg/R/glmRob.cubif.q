glmRob.cubif <- function(x, y, intercept = FALSE, offset = 0.0,
  family = binomial(), null.dev = TRUE, control) 
{
  the.call <- match.call()

  # figure out a more elegant way to do this #
  robust.cov.control <- control$init.cov

# family <- as.family(family)
# nn <- family$family["name"]
  nn <- casefold(family$family)

  if(nn == "gaussian")
    stop("Use lmRob(formula, ...) for the gaussian case")

  ics <- 0

  if(nn == "binomial") {
    ics <- 2

    if(is.matrix(y)) {
      if(dim(y)[2] > 2)
        stop("only binomial response matrices (2 columns)")

      ni <- as.vector(y %*% c(1,1))
      y <- y[,1]
      ly <- length(y)
    }

    else {
      ics <- 1

      if(is.factor(y))
        y <- as.numeric(y != levels(y)[1])
      else
        y <- as.vector(y)

      if(any(y > 1))
        stop("Response doesn't look Binomial")

      ly <- length(y)
      ni <- rep(1, ly)
    } 
  }

  if(nn == "poisson") {

    if(!is.null(dim(y)[2]))
      stop("Poisson response cannot be a matrix")

#    eval(family$initialize)

    ics <- 3 
    ly <- length(y)
    ni <- rep(1,ly)
  }

  if(ics == 0)
    stop(paste(nn, "family not implemented in glmRob yet", sep=" "))

  eta <- ci <- ai <- rsdev <- y 
  yy <- y 
  dn <- dimnames(x)
  xn <- dn[[2]]
  yn <- dn[[1]]

  if(intercept)
    x <- cbind(1,x)

  if(is.null(offset) || length(offset) <= 1)
    offset <- rep(0.0, ly) 

  p <- ncol(x)
  n <- nrow(x)

  zero <- rep(FALSE, n)

#
# Initializations
#

  qrx <- qr(x)[c("qr", "rank", "pivot", "qraux")]
  rank <- qrx$rank
  piv <- 1:rank
  epsilon <- control$epsilon
  tua <- control$epsilon
  maxit <- control$maxit
  mxt <- maxit
  mxf <- mxt
  gma <- control$gma
  iug <- control$iug
  ipo <- control$ipo
  ilg <- control$ilg
  icn <- control$icn
  icv <- control$icv
  ia1 <- control$ia1
  trc <- control$trc

  gma <- 1
  iug <- 1
  ipo <- 1
  ilg <- 1
  icn <- 1
  icv <- 1
  ia1 <- 1

# tmp <- list(...)$singular.ok
#
# if(!is.null(tmp))
#    singular.ok <- tmp
#  else
#    singular.ok <- FALSE
#
# tmp <- list(...)$qr.out
#
# if(!is.null(tmp))
#    qr.out <- tmp
#  else
#    qr.out <- FALSE

  singular.ok <- FALSE
  qr.out <- FALSE

  if(rank < p) {
 
    if(!singular.ok)
      stop(paste("x is singular, rank(x)=", rank))

    else {
      piv <- qrx$pivot[1:rank]
      x <- x[ , piv, drop = FALSE]

      if(any(zero))
        x0 <- x0[ , piv, drop = FALSE]

      xn <- dimnames(x)[[1]] 
    }
  }

  bpar <- control$bpar
  cpar <- control$cpar

# Deviance for the model reduced to the constant term.

  if(null.dev)
    null.dev <- glmRob.cubif.null(x, y, ni, offset, ics, family, control, robust.cov.control)

  else
    null.dev <- NULL 

# Initial theta, A (A0) and c (c0)

  zi <- glmRob.cubif.Ini(X = x,
                         y = y,
                         ni = ni,
                         icase = ics,
                         offset = offset,
                         b = bpar,
                         zmin = 0.001,
                         epsilon = epsilon,
                         ia1 = ia1,
                         control = robust.cov.control)

  ci <- zi$ci
  cov <- zi$cov
  ai <- zi$ai
  theta0 <- theta <- zi$theta

  if(icn != 1) 
    stop("icn != 1 in glmRob.cubif")

  if(trc)
    cat("\nFull model.\n")

  sqcov <- rep(1.0, p)

  for(i in 1:p) {
    j <- (i*(i+1))/2; 
    sqcov[i] <- sqrt(cov[j])
  }

# Iterations

  nit <- 1
  repeat {

# theta-step

    zt <- glmRob.gytstp(x, y, ci, theta, ai, cov, ni, offset, tol = epsilon,
      icase = ics, maxit = mxt)

    theta  <- zt$theta[1:p]
    vtheta <- zt$vtheta
    nitt <- zt$nit

# Check convergence

      if(nit == maxit)
        break

      delta <- theta - theta0

      if(all(epsilon*sqcov - abs(delta) > 0))
        break

      theta0 <- theta

# c-step

    zc <- glmRob.gicstp(icase = ics, ialg = ilg, ni = ni, vtheta = vtheta,
      ai = ai, oi = offset, tol = epsilon, maxit = mxt)

    ci <- zc$ci
    nit <- nit+1
  }

# End Iterations

# Final covariance matrix of estimated coefficients

  z <- glmRob.gfedca(vtheta, ci, ai, ni, offset, icase = ics)
  zc <- glmRob.ktaskw(x = x, d = z$d, e = z$e, f = 1/n, f1 = 1, iainv = 0,
    ia = ia1, tau = epsilon)

  covf <- zc$cov
  zf <- list(theta = theta, ai = ai, vtheta = vtheta, ci = ci, cov = covf,
    nit = nit, delta = delta)

  coefs <- zf$theta

#  Residual Deviance

  zd <- glmRob.glmdev(y, ni, zf$ci, zf$ai, zf$vtheta, offset, icase = ics)

  cov <- matrix(0, nrow=rank, ncol=rank)
  i2 <- 0

  for(i in 1:rank) {
    i1 <- i2 + 1
    i2 <- i1 + i - 1
    cov[i,1:i] <- zf$cov[i1:i2]
    cov[1:i,i] <- zf$cov[i1:i2]
  }


  xn <- dimnames(x)[[2]]
  names(coefs) <- xn
  dimnames(cov) <- list(xn,xn)
  asgn <- attr(x, "assign")

# Compute eta, mu and residuals.

  ind <- 1:n
  dni <- c(ind[1],diff(ind)) 
  iii <- cumsum(dni[dni!=0])
  jjj <- (1:n)*(dni!=0) 
  eta[iii] <- zf$vtheta[jjj]

  #if(any(offset))
  offset[iii] <- offset[jjj]

  ci[iii] <- zf$ci[jjj]
  ai[iii] <- zf$ai[jjj] 
  rsdev[iii] <- zd$li[jjj] - zd$sc[jjj]
  dni <- ni 
  ni <- rep(1,length(eta))
  ni[iii] <- dni[jjj] 

# mu <- family$inverse(eta+offset)
  mu <- family$linkinv(eta+offset)

  names(eta) <- yn

  if(rank < p) {
    coefs[piv[ - seq(rank)]] <- NA
    pasgn <- asgn
    newpos <- match(1:p, piv)
    names(newpos) <- xn

    for(j in names(asgn)) {
      aj <- asgn[[j]]
      aj <- aj[ok <- (nj <- newpos[aj]) <= rank]

      if(length(aj)) {
        asgn[[j]] <- aj
        pasgn[[j]] <- nj[ok]
      }
      else
        asgn[[j]] <- pasgn[[j]] <- NULL
    }

    cnames <- xn[piv]
  }
  
# new.dev <- family$deviance(mu, yy/ni, w = rep(1.0, n))
  new.dev <- sum(family$dev.resids(yy/ni, mu, w = rep(1.0, n)))

  resp <- yy/ni - mu
  names(ai) <- yn
  names(mu) <- yn
  df.residual <- ly - rank

  fit <- list(coefficients = coefs, fitted.values = mu, ci = ci, rank = rank,
    assign = asgn, df.residual = df.residual, control = control)

  if(rank < p) {
    if(df.residual > 0)
      fit$assign.residual <- (rank + 1):n
  }
  
  if(qr.out)
    fit$qr <- qrx

  fit$ai <- ai
  fit$cov <- cov
  fit$ni <- ni
  rsdev <- sign(y-mu)*sqrt(2*abs(rsdev)) 
  fit$weights <- pmin(1, fit$ai / abs(yy/ni-ci-mu))

  names(fit$fitted) <- names(eta)
  
  c(fit, list(family = family, ics=ics, linear.predictors = eta, deviance = new.dev,
    null.deviance = null.dev, call = the.call, iter = nit, y = yy/ni,
    contrasts = attr(x,"contrasts"), rsdev = rsdev, gradient = zf$grad,
    inv.hessian = zf$hessnv, residuals = yy/ni - ci - mu, wa = zf$ai,
    vtheta = zf$vtheta))
}


