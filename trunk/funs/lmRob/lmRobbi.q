#----------------------------------------------------------------#
# Robust Linear Regression (bounded influence estimates)         #
# Author: Jeffrey Wang                                           #
# Date  : 10/25/1999                                             #
# DAPD of MathSoft, Inc.                                         #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# LM-like functions                                              #
#----------------------------------------------------------------#

"lmRobBI" <- function(formula, data, weights, subset, na.action, 
    model = F, x = F, y = F, contrasts = NULL, ...)
{
  call <- match.call()
  m <- match.call(expand = F)
  m$method <- m$model <- m$x <- m$y <- m$contrasts <- m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, sys.parent())
  Terms <- attr(m, "terms")
  weights <- model.extract(m, weights)
  Y <- model.extract(m, response)
  X <- model.matrix(Terms, m, contrasts)
  fit <- if(length(weights)) lmRobBI.wfit(X, Y, weights, ...) else 
	lmRobBI.fit(X, Y, ...)
  fit$terms <- Terms
  fit$call <- call
  if(model) fit$model <- m
  if(x)  fit$x <- X
  if(y) fit$y <- Y
  attr(fit, "na.message") <- attr(m, "na.message")

  if (!is.null(attr(m, "na.action")))
    fit$na.action <- attr(m, "na.action")

  fit
}

"lmRobBI.fit" <- function(x, y, ...)
{
  if(!is.numeric(x))
    stop("model matrix must be numeric")
  if(!is.numeric(y))
    stop("response must be numeric")

  fit <- lmRobBI.fit.S(x, y, ...)
  fit$contrasts <- attr(x, "contrasts")
  fit
}

"lmRobBI.wfit" <- function(x, y, w, ...)
{
  if(!is.numeric(x))
    stop("model matrix must be numeric")
  if(!is.numeric(y))
    stop("response must be numeric")
  if(any(w < 0))
    stop("negative weights not allowed")
  contr <- attr(x, "contrasts")
  zero <- w == 0
  multiy <- length(dim(y)) == 2
  if(any(zero)) {
    pos <- !zero
    r <- f <- y
    ww <- w
    x0 <- x[zero,  , drop = F]
    y0 <- if(multiy) y[zero,  , drop = F] else y[zero]
    x <- x[pos,  , drop = F]
    y <- if(multiy) y[pos,  , drop = F] else y[pos]
    w <- w[pos]
  }
  w.factor <- w^0.5
  x <- x * w.factor
  y <- y * w.factor
  fit <- lmRobBI.fit(x, y, ...)
  fit$residuals <- fit$residuals/w.factor
  fit$fitted.values <- fit$fitted.values/w.factor
  if(any(zero) && method != "null") {
    nas <- is.na(fit$coef)
    if(any(nas))
    f0 <- x0[, !nas] %*% fit$coef[!nas]
    else f0 <- x0 %*% fit$coef
    if(multiy) {
      r[pos,  ] <- fit$resid
      f[pos,  ] <- fit$fitted
      r[zero,  ] <- y0 - f0
      f[zero,  ] <- f0
    }
    else {
      r[pos] <- fit$resid
      f[pos] <- fit$fitted
      r[zero] <- y0 - f0
      f[zero] <- f0
    }
    fit$residuals <- r
    fit$fitted.values <- f
    w <- ww
  }
  fit$weights <- w
  fit$contrasts <- contr
  fit
}

"lmRobBI.fit.S" <- function(x, y, control=lmRobBI.control(), method="sch",
  singular.ok=T, ...)
{
  if (!is.matrix(x)) {
    x <- array(c(rep(1,length(x)), x), c(length(x), 2), 
	       list(names(x),c("(Intercept)", deparse(substitute(x)))))
  }
  if(match("(Intercept)", dimnames(x)[[2]], nomatch = 0) == 0)
    intercept <- F
  else 
    intercept <- T
  n    <- nrow(x)
  np   <- ncol(x)
  ncov <- np*(np+1)/2
  effc <- control$efficiency
  tuning.u <- control$tuning.p
  tmp <- lmRobBI.eff(effc, method=method, tuning.p=tuning.u, 
                     intercept=T, nx=np-intercept)
  max.wt   <- control$max.wt
  max.rg   <- control$max.rg
  alg      <- control$alg
  tol      <- control$tol
  tau      <- control$tau
##
  qrx <- qr(x)[c("rank", "pivot")]
  rank <- qrx$rank
  piv <- qrx$pivot[1:rank]
  if(rank < np) {
    if(!singular.ok) 
      stop(paste("x is singular, rank(x)=", rank))
    else {
      x <- x[, piv]
      outList <- list(coefficients=rep(NA,np), cov=NA, 
                      residuals=rep(NA,n), O.weights=rep(NA,n), 
                      fitted.values=rep(NA,n), rank=rank)
    }
  }
  else {
    itypw <- 1
    if (method == "sch" || method == "SCH" || method == "Sch") {
      iucv <- 3
      iwww <- 1
      itype <- 3
      tuning.a <- tuning.u <- tmp$tuning.s
      tuning.b <- 0
    }
    else if (method == "mal" || method == "MAL" || method == "Mal") {
      iucv <- 1
      iwww <- 3
      itype <- 2
      tuning.a <- 0
      tuning.b <- tmp$tuning.p
      tuning.u <- tmp$tuning.s
    }
    else {
      stop(paste("Available methods for bounded influence",
                 " estimators are: sch and mal.", sep=""))
    } 
    storage.mode(x) <- storage.mode(y) <- "double"
##......................................................................
## Compute scatter matrix A using Newton algorithm
##......................................................................
    z <- .Fortran("s_wedvbi",
                  x,
                  as.integer(np),
                  as.integer(ncov),
                  as.integer(n),
                  as.integer(itypw),
                  init=as.integer(2),
                  as.integer(n),
                  a=double(ncov),
                  double(n))$a
    z <- .Fortran("s_wnagbi",
                  x=x,
                  a=z,
                  as.integer(n),
                  as.integer(np),
                  as.integer(ncov),
                  as.integer(n),
                  as.integer(max.wt),
                  icnv=as.integer(1),
                  as.double(tol),
                  xfud=as.double(1),
                  nit=integer(1),
                  dist=double(n),
                  sa=double(ncov),
                  ss=double(ncov),
                  su=double(n),
                  sup=double(n),
                  st=double(ncov),
                  sd=double(np),
                  iucv=as.integer(iucv),
                  a2=as.double(tuning.a),
                  b2=as.double(tuning.b))
    nitw <- z$nit
    wgt  <- www.weight(z$dist,iwww=iwww,iucv=iucv,c1=tuning.a,c2=tuning.b)
##.....................................................................
## Initial cov, theta and sigma
##.....................................................................
    cov <- lmRobBI.cov0(wgt, x, tuning.u=tuning.u, itype=itype, tau=tau)
    z   <- lmRobBI.bRobust(x, y)
    theta0 <- z$theta
    sigma0 <- z$sigma
##.....................................................................
## Final cov, theta and sigma
##.....................................................................
    z <- lmRobBI.ts1(x, y, wgt, tuning.u=tuning.u, itype=itype, tau=tau,
                     tol=tol, theta0=theta0, sigma0=sigma0, cov=cov,
                     alg=alg, max.rg=max.rg)
    theta1 <- z$theta1
    sigma1 <- z$sigma1
    res1   <- z$res
    nit1   <- z$nit1
    fval   <- y - res1
    cov <- lmRobBI.cov1(wgt, x, res1, sigma1=sigma1, tuning.u=tuning.u,
                        itype=itype, tau=tau)
    mcov <- matrix(0, np, np)
    tmp <- dimnames(x)
    tmp[[1]] <- tmp[[2]]
    dimnames(mcov) <- tmp
    xn <- tmp[[2]]
    names(theta1) <- xn
    attributes(res1) <- attributes(y)
    nl <- 0
    for (i in 1:np) {
      for (j in 1:i) {
        nl <- nl + 1
        mcov[i,j] <- cov[nl]
        if (i != j)
          mcov[j,i] <- cov[nl]
      }
    }
    outList <- list(coefficients=theta1, scale=sigma1, iter.weight=nitw,
                    iter.coef=nit1, cov=mcov, residuals=res1, 
                    fitted.values=fval, rank=rank, O.weights=wgt)
  }

  asgn <- attr(x, "assign")
  pasgn <- asgn
  newpos <- match(1:rank, piv)
  if(length(xn)) names(newpos) <- xn
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
  outList$assign <- asgn
  oldClass(outList) <- "lmRobBI"
  outList
}

#--------------------------------------------------------------#
# Control List                                                 #
#--------------------------------------------------------------#

"lmRobBI.control" <- function(efficiency=0.95, tuning.p=NULL, 
  max.wt=150, max.rg=30, alg="W", tol=1e-5, tau=1e-11)
{
  list(efficiency=efficiency, tuning.p=tuning.p, max.wt=max.wt, 
       tol=tol, tau=tau, max.rg=max.rg, alg=alg)
}

#-------------------------------------------------------------#
# Access Functions                                            #
#-------------------------------------------------------------#

"coef.lmRobBI" <- function(object,...) {
  oldClass(object) <-"lm"
  UseMethod("coef")
}
 
"formula.lmRobBI" <- function(object,...) {
  oldClass(object) <-"lm"
  UseMethod("formula")
}

"residuals.lmRobBI" <- function(object,...) {
  oldClass(object) <-"lm"
  UseMethod("residuals")
}
 
"model.frame.lmRobBI" <- function(formula,...) {
  oldClass(formula) <-"lm"
  UseMethod("model.frame")
}
 
"model.matrix.lmRobBI" <- function(object,...) {
  oldClass(object) <-"lm"
  UseMethod("model.matrix")
}

"weights.lmRobBI" <- function(x) {
  stop("Not implemented yet.")
}

#--------------------------------------------------------------#
# Methods for Generic Functions                                #
#--------------------------------------------------------------#

"predict.lmRobBI" <- function(x) {
  stop("Not implemented yet.")
}

"plot.lmRobBI" <- function(x, residuals = NULL, smooths = F, 
  rugplot = F, id.n = 3, ask = F, which.plots = NULL, ...) {
#
# Identical to plot.lmRobBI
#
  Residuals <- resid(x)
  if(!is.null(dim(Residuals)))
    stop("Not implemented for multivariate responses.")
  if(!is.null(residuals)) {
    if(length(residuals) == 1 && residuals) 
      residuals <- Residuals
    else 
      Residuals <- residuals
  }
  fits <- predict.lm(x)
  response <- fits + Residuals
  form <- formula.lm(x)
  response.name <- deparse(form[[2]])
  model <- deparse(form[[3]])
  fit.lab <- paste("Fitted :", model, sep = " ")

  add.ons <- function(x, y, smooths = T, rugplot = T, id.n = 3) {
    if(smooths) {
      prediction <- loess.smooth(x, y, span = 1, degree = 1)
      lines(prediction)
    }
    if(rugplot) {
      jx <- jitter(x[!is.na(x)])
      xlim <- range(jx)
      rug(jx)
    }
    if(id.n) {
# Identify id.n greatest y-values (in absolute value)
      n <- length(y)
      oy <- order(abs(y))
      which <- oy[(n - id.n + 1):n]
      text(x[which], y[which], as.character(which), adj = 0)
    }
  }

  if (is.null(which.plots)) {
    choices <- c("All", "Residuals vs Fitted Values", 
                 "Sqrt of abs(Residuals) vs Fitted Values", 
                 "Response vs Fitted Values", "Normal QQplot of Residuals", 
                 "r-f spread plot")
    choices <- substring(choices, 1, 40)	#truncate long names
    tmenu <- paste("plot:", choices)
    pick <- 2
    ask.now <- ask
    while(pick <= length(tmenu) + 2) {
      if(ask.now)
        pick <- menu(tmenu, title = 
                "\nMake a plot selection (or 0 to exit):\n") + 1
      switch(pick,
        invisible(return(x)),
        {
# Plot all choices one by one
          ask.now <- F
        },
        {
# Residuals vs Fitted Values
          plot(fits, Residuals, xlab = fit.lab, ylab = "Residuals", ...)
          abline(h = 0, lty = 2)
          add.ons(fits, Residuals, smooths = smooths, rugplot = rugplot, 
                  id.n = id.n)
        },
        {
# Sqrt of abs(Residuals) vs Fitted Values
          y <- sqrt(abs(Residuals))
          plot(fits, y, xlab = fit.lab, 
               ylab = deparse(substitute(sqrt(abs(resid(lm.obj))))), ...)
          add.ons(fits, y, smooths = smooths, rugplot = rugplot, id.n = id.n)
        },
        {
# Response vs Fitted Values
          plot(fits, response, xlab = fit.lab, ylab = response.name, ...)
          abline(0, 1, lty = 2)
          add.ons(fits, response, smooths = smooths, rugplot = rugplot, 
                  id.n = F)
        },
        {
# Normal QQplot of Residuals
          qqxy <- qqnorm(Residuals)
          add.ons(qqxy$x, qqxy$y, smooths = F, rugplot = F, id.n = id.n)
          qqline(Residuals, lty = 2)
        },
        {
# Plot an r-f spread plot
          rfplot(fits, Residuals, yname = response.name)
        }
      )
      if(!ask.now) pick <- pick + 1
      if(pick == length(tmenu) + 2) ask.now <- ask
    }
  }
  else {
    for (i in which.plots) {
      switch(i,
        {
# Residuals vs Fitted Values
          plot(fits, Residuals, xlab = fit.lab, ylab = "Residuals", ...)
          abline(h = 0, lty = 2)
          add.ons(fits, Residuals, smooths = smooths, rugplot = rugplot, 
                  id.n = id.n)
        },
        {
# Sqrt of abs(Residuals) vs Fitted Values
          y <- sqrt(abs(Residuals))
          plot(fits, y, xlab = fit.lab, 
               ylab = deparse(substitute(sqrt(abs(resid(lm.obj))))), ...)
          add.ons(fits, y, smooths = smooths, rugplot = rugplot, id.n = id.n)
        },
        {
# Response vs Fitted Values
          plot(fits, response, xlab = fit.lab, ylab = response.name, ...)
          abline(0, 1, lty = 2)
          add.ons(fits, response, smooths = smooths, rugplot = rugplot, 
                  id.n = F)
        },
        {
# Normal QQplot of Residuals
          qqxy <- qqnorm(Residuals)
          add.ons(qqxy$x, qqxy$y, smooths = F, rugplot = F, id.n = id.n)
          qqline(Residuals, lty = 2)
        },
        {
# Plot an r-f spread plot
          rfplot(fits, Residuals, yname = response.name)
        },
        warning(paste("There is no plot number", i))
      )
    }
  }
  invisible(x)
}

"print.lmRobBI" <- function(x, ...) {
  if(!is.null(cl <- x$call)) {
    cat("Call:\n")
    dput(cl)
  }
  coef <- coefficients(x)
  if(ns <- attr(coef, "singular"))
    cat("\nCoefficients: (", ns, 
        " not defined because of singularities)\n", sep = "")
  else 
    cat("\nCoefficients:\n")
  print(coef, ...)
  rank <- x$rank
  nobs <- length(x$residuals)
  rdf <- nobs - rank
  cat("\nDegrees of freedom:", nobs, "total;", rdf, "residual\n")
  if (!is.null(x$na.action))
    cat(naprint(x$na.action),"\n")
  if(rdf > 0) {
    if(is.null(w <- x$weights))
      cat("Residual scale estimate:", format(x$scale), "\n")
    else 
      cat("Residual scale estimate (on weighted scale):", 
          format(x$scale), "\n")
  }
  invisible(x)
}

"print.summary.lmRobBI" <- function(x, digits=max(3,.Options$digits-3), ...) 
{
  cat("\nCall: "); dput(x$call)
  resid <- x$residuals
  df <- x$df
  rdf <- df[2]
  if(rdf > 5) {
    cat("\nResiduals:\n")
    if(length(dim(resid)) == 2) {
      rq <- apply(t(resid), 1, quantile)
      dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                             "Max"), dimnames(resid)[[2]])
    }
    else {
      rq <- quantile(resid)
      names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
    }
    print(rq, digits = digits, ...)
  }
  else if(rdf > 0) {
    cat("\nResiduals:\n")
    print(resid, digits = digits, ...)
  }
  if(nsingular <- df[3] - df[1])
    cat("\nCoefficients: (", nsingular, 
        " not defined because of singularities)\n", sep = "")
  else 
    cat("\nCoefficients:\n")
  print(format(round(x$coef, digits = digits)), quote = F, ...)
  cat("\nResidual scale estimate:", format(signif(x$sigma, digits)), 
      "on",rdf, "degrees of freedom\n")
  if(!is.null(x$na.action))
    cat(naprint(x$na.action),"\n")
##
##  cat("\nProportion of variation in response explained by model:", 
##      format(signif(x$r.squared, digits)),"\n")
##
  correl <- x$correlation
  if(!is.null(correl)) {
    p <- dim(correl)[2]
    if(p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      ll <- lower.tri(correl)
      correl[ll] <- format(round(correl[ll], digits), ...)
      correl[!ll] <- ""
      print(correl[-1,-p,drop=F],quote=F,digits=digits,...)
    }
  }
  invisible(x)
}

"summary.lmRobBI" <- function(object, correlation = T) {
  wt <- object$M.weights
  wt1 <- object$weights
  if(!is.null(wt1))
    wt <- wt * wt1
  coef <- coefficients(object)
  cnames <- labels(coef)
  ctotal <- object$coef
  ptotal <- length(ctotal)
  resid <- object$residuals
  fv <- object$fitted
  n <- length(resid)
  p <- object$rank
  if(is.null(p)) p <- sum(!is.na(ctotal))
  if(any(na <- is.na(coef))) {
    coef <- coef[!na]
    p <- length(coef)
  }
  rdf <- n - p
  if(!is.null(wt1)) {
    wt1 <- wt1^0.5
    resid <- resid * wt1
    fv <- fv * wt1
    excl <- wt1 == 0
    if(any(excl)) {
      warning(paste(sum(excl),"rows with zero weights not counted"))
      resid <- resid[!excl]
      fv <- fv[!excl]
      wt1 <- wt1[!excl]
      if(is.null(object$df.residual))
        rdf <- rdf - sum(excl)
      wt <- wt * wt1
    }
  }
  stddev <- object$scale
  cov <- object$cov
  var <- diag(cov)
  if(p < ptotal)
    R <- R[1:p, 1:p, drop = F]
  if(correlation) {
    correl <- cov/sqrt(var)
    correl <- t(correl)/sqrt(var)
  }
  else 
    correl <- NULL
  coef <- array(coef, c(p, 4))
  dimnames(coef) <- list(cnames, c("Value", "Std. Error", "t value", 
                         "Pr(>|t|)"))
  coef[, 2] <- sqrt(var)
  coef[, 3] <- coef[, 1]/coef[, 2]
  coef[, 4] <- if(rdf > 0) 2 * (1 - pt(abs(coef[, 3]), rdf)) else NA
  yy <- fv + resid
  int <- attr(object$terms, "intercept")
  object <- object[c("call", "terms", "iter.coef", "iter.weight",  
                     "O.weights", "na.action")]
  object$residuals <- resid
  object$coefficients <- coef
  object$sigma <- stddev
  object$df <- c(p, rdf, ptotal)
  object$cov.unscaled <- cov/stddev^2
  object$correlation <- correl
  oldClass(object) <- c("summary.lmRobBI", "summary.lm")
  object
}

#--------------------------------------------------------------#
# Utility Functions                                            #
#--------------------------------------------------------------#

"lmRobBI.eff" <- function(eff, method="sch", tuning.p=NULL, nx=1, 
  intercept=T, sigmax=1.0) {
##------------------------------------------------------------------
## given a desired efficiency level, calculates the tuning constant
##------------------------------------------------------------------
  eff.sch.func <- function(cc, eff, mu=1, ialfa=1, sigmx=1)
  {
    lmRob.eff0(mu=mu,itype=3,iucv=3,iwww=1,ialfa=ialfa,sigmx=sigmx,
                 ta=cc,tc=cc)$reff-eff
  } 
  eff.mal.func <- function(cc, eff, mu=1, ialfa=1, sigmx=1, tuning.p=NULL)
  {
    lmRob.eff0(mu=mu,itype=2,iucv=1,iwww=3,ialfa=ialfa,sigmx=sigmx,
                 tb=tuning.p,tc=cc)$reff-eff
  } 
  if (method == "sch") {
    c.inv <- c(1.05*sqrt(nx+intercept), 50)
    out <- uniroot(eff.sch.func, interval=c.inv, eff=eff, mu=nx,
                   ialfa=intercept, sigmx=sigmax)$root
  }
  else {# method == "mal"
    c.inv <- c(0.1, 5)
    if (is.null(tuning.p) || tuning.p < sqrt(nx+intercept)) 
      tuning.p <- 1.7*((nx+intercept)^(1/3))
    out <- uniroot(eff.mal.func, interval=c.inv, eff=eff, mu=nx,
                   ialfa=intercept, sigmx=sigmax, tuning.p=tuning.p)$root
  }
  list(tuning.s=out, tuning.p=tuning.p)
}

"www.weight" <- function(svals, iwww=1, iucv=3, c1=1.6, c2=0.0)
{
  n <- length(svals)
  storage.mode(svals) <- "double"
  f.res <- .Fortran("s_wwwabi",
                    as.integer(n),
                    svals,
                    fvals = double(n),
                    as.integer(iwww),
                    as.integer(iucv),
                    as.double(c1),
                    as.double(c2))
  f.res$fvals
}

"lmRobBI.bRobust" <- function(x, y, control=lmRobBI.control())
{
  n    <- nrow(x) 
  np   <- ncol(x)
  ncov <- np*(np+1)/2
  tuning.c <- 1.5
  maxit    <- control$max.wt
  tau <- control$tau
  tlo <- control$tol
  storage.mode(x) <- storage.mode(y) <- "double"
##.....................................................................
## Compute scatter matrix A using fixed point algorithm
##.....................................................................
  itypw <- 1
  z <- .Fortran("s_wedvbi",
                x,
                as.integer(np),
                as.integer(ncov),
                as.integer(n),
                as.integer(itypw),
                init=as.integer(2),
                as.integer(n),
                a=double(ncov),
                double(n))$a
  z <- .Fortran("s_wfagbi",
                x,
                a=z,
                y,
                as.integer(n),
                as.integer(np),
                nvarq=as.integer(0),
                as.integer(ncov),
                as.integer(n),
                as.double(tau),
                as.integer(maxit),
                icnv=as.integer(1),
                as.integer(itypw),
                igwt=as.integer(0),
                as.double(tlo),
                nit=integer(1),
                dist=double(n),
                su=double(n),
                sa=double(ncov),
                st=double(ncov),
                sd=double(np),
                sz=double(np),
                iucv=as.integer(5),
                as.double(4.0),
                as.double(0.0))
  nitw <- z$nit
  wgt  <- 1/z$dist
##.....................................................................
## Least Absolute Residuals
##.....................................................................
  itype <- 2
  storage.mode(wgt) <- "double"
  z <- .Fortran("s_bet0bi",
                wgt=wgt,
                as.integer(n),
                as.integer(itype),
                isqw=as.integer(0),
                as.double(tlo),
                bt0=double(1))$bt0
  xt <- x * wgt
  yt <- y * wgt
  mdt <- max(n,np)
  storage.mode(xt) <- storage.mode(yt) <- "double"
  z <- .Fortran("s_larsbi",
                x=xt,
                y=yt,
                as.integer(n),
                as.integer(np),
                as.integer(n),
                as.integer(mdt),
                as.double(tau),
                nit=integer(1),
                k=integer(1),
                kode=integer(1),
                sigma=double(1),
                theta=double(mdt),
                rs=double(n),
                sc1=double(n),
                sc2=double(np),
                sc3=double(np),
                sc4=double(np),
                as.double(z))
  theta0 <- z$theta  
  sigma0 <- z$sigma
  rs     <- z$rs/wgt
  r1     <- rs/sigma0 
  z <- .Fortran("s_epshbi",
                as.double(tuning.c),
                epsi2=double(1),
                epsip=double(1))
  den <- z$epsip
  g   <- psp.weight(r1,ips=3,xk=tuning.c)/den
  list(theta=theta0, sigma=sigma0, rs=rs, g=g, nitw=nitw)
}

"lmRobBI.cov0" <- function(wgt, x, tuning.u=1.345, itype=2, tau=1e-11)
{
  n    <- nrow(x)
  np   <- ncol(x)
  ncov <- np*(np+1)/2
  storage.mode(wgt) <- storage.mode(x) <- "double"
  z <- .Fortran("s_kedhbi",
                wgt=wgt,
                as.integer(n),
                as.double(tuning.u),
                as.integer(itype),
                d=double(n),
                e=double(n))
  f <- 1/n
  sc <- matrix(double(1),np,np)
  cov <- .Fortran("s_ktasbi",
                  x,
                  z$d,
                  z$e,
                  as.integer(n),
                  as.integer(np),
                  as.integer(n),
                  as.integer(np),
                  as.integer(ncov),
                  as.double(tau),
                  as.integer(1),
                  as.double(f),
                  as.double(0),
                  iainv=as.integer(1),
                  a=double(ncov),
                  s1inv=double(ncov),
                  s2=double(ncov),
                  ainv=double(ncov),
                  cov=double(ncov),
                  sc)$cov
  cov
}

"lmRobBI.ts1" <- function(x, y, wgt, isigma=2, tuning.u=1.345, itype=2, 
  tau=1e-11, tol=1e-5, theta0=NULL, sigma0=NULL, cov=NULL, alg="N", 
  max.rg=50)
{
  n    <- nrow(x)
  np   <- ncol(x)
  ncov <- np*(np+1)/2
  storage.mode(wgt) <- storage.mode(x) <- storage.mode(y) <- "double"
  if (isigma == 1) {
    beta  <- .Fortran("s_bethbi",
                     wgt,
                     as.integer(n),
                     d=as.double(tuning.u),
                     as.integer(itype),
                     beta=double(1))$beta
    bet0 <- 0
  }
  else {
    bet0 <- .Fortran("s_bet0bi",
                     wgt,
                     as.integer(n),
                     as.integer(itype),
                     as.integer(1),
                     as.double(tol),
                     bet0=double(1))$bet0
    beta <- 0
  }
  mdt <- max(n,np) 
  lth <- length(theta0)
  if (lth < mdt) 
    theta0 <- c(theta0, rep(0, mdt-lth))
  sx <- matrix(double(1), n, np)
  storage.mode(theta0) <- "double"
  if (alg == "N" || alg == "n") {
    z <- .Fortran("s_rnagbi",
                  x,
                  y,
                  theta=theta0,
                  wgt,
                  cov,
                  sigmai=as.double(sigma0),
                  as.integer(n),
                  as.integer(np),
                  as.integer(n),
                  as.integer(mdt),
                  as.integer(ncov),
                  gam=as.double(1.0),
                  as.double(tol),
                  as.double(tau),
                  as.integer(itype),
                  iopt=as.integer(2),
                  as.integer(isigma),
                  icnv=as.integer(1),
                  as.integer(max.rg),
                  mxs=as.integer(1),
                  nit=integer(1),
                  sigmaf=double(1),
                  qs1=double(1),
                  rs=double(n),
                  delta=double(np),
                  grad=double(np),
                  hessnv=double(ncov),
                  sd=double(n),
                  sw=double(n),
                  sf=double(np),
                  sg=double(np),
                  sh=double(np),
                  ip=integer(np),
                  sx,
                  ips=as.integer(3),
                  xk=as.double(tuning.u),
                  beta=as.double(beta),
                  bet0=as.double(bet0))
  }
  else { # alg == W
    psp0 <- psp.weight(0,ips=3,xk=tuning.u)
    z <- .Fortran("s_rwagm2",
                  x,
                  y,
                  theta=theta0,
                  wgt,
                  cov,
                  as.double(psp0),
                  sigmai=as.double(sigma0),
                  as.integer(n),
                  as.integer(np),
                  as.integer(n),
                  as.integer(ncov),
                  as.double(tol),
                  gam=as.double(1),
                  as.double(tau),
                  as.integer(itype),
                  as.integer(isigma),
                  icnv=as.integer(1),
                  mxt=as.integer(max.rg),
                  mxs=as.integer(1),
                  nit=integer(1),
                  sigmaf=double(1),
                  rs=double(n),
                  delta=double(np),
                  sc=double(n),
                  sf=double(np),
                  sg=double(np),
                  sh=double(np),
                  ip=integer(np),
                  double(n),
                  sx,
                  ips=as.integer(3),
                  xk=as.double(tuning.u),
                  beta=as.double(beta),
                  bet0=as.double(bet0))
  }
  list(theta1=z$theta[1:np], sigma1=z$sigmaf, res=z$rs, nit1=z$nit)
}

"lmRobBI.cov1" <- function(wgt, x, res, sigma1=NULL, tuning.u=1.345, 
  itype=2, tau=1e-11)
{
  n    <- nrow(x)
  np   <- ncol(x)
  ncov <- np*(np+1)/2
  storage.mode(wgt) <- storage.mode(x) <- storage.mode(res) <- "double"
  z <- .Fortran("s_kedcbi",
                wgt,
                res,
                as.integer(n),
                as.double(sigma1),
                as.integer(itype),
                d=double(n),
                e=double(n),
                ips=as.integer(3),
                xk=as.double(tuning.u))
  f <- sigma1^2/n
  sc <- matrix(double(1),np,np)
  cov <- .Fortran("s_ktasbi",
                  x,
                  z$d,
                  z$e,
                  as.integer(n),
                  as.integer(np),
                  as.integer(n),
                  as.integer(np),
                  as.integer(ncov),
                  as.double(tau),
                  as.integer(1),
                  as.double(f),
                  as.double(0),
                  iainv=as.integer(1),
                  a=double(ncov),
                  s1inv=double(ncov),
                  s2=double(ncov),
                  ainv=double(ncov),
                  cov=double(ncov),
                  sc)$cov
  cov
}

