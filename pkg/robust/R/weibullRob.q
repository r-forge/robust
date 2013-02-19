weibullRob <- function(x, estim = c("M", "tdmean"),
                       control = weibullRob.control(estim, ...), ...)
{
	estim <- match.arg(estim)
	the.call <- match.call()
  data <- x
  data.name <- deparse(substitute(x))

  if(estim == "M") {
    maxit <- control$maxit
    tol <- control$tol
    til <- control$til
    sigma <- control$sigma
    vcov <- control$vcov
    b1 <- control$b1
    b2 <- control$b2
    A <- control$A

    Table <- Tab.weibull(b1 = b1, b2 = b2, A = A, maxit = maxit,
                         til = til, tol = tol)

    nobs <- length(x)
    x[x == 0] <- 0.5
    x <- sort(x)

    tab <- Table$Table

    # alf* came from the call to Tab.weibull in the original code - since
    # there are no args named alpha1 and alpha2 I just hard coded them here

    alf1 <- 0.4
    alf2 <- 10.4
    k <- 1

    tpar <- c(b1, b2, alf1, alf2, k, alf2 - alf1)
    x <- log(x)

    f.res <- .Fortran("rlwestim",
                      x = as.double(x),
                      nobs = as.integer(nobs),
                      tab = as.double(tab),
                      maxit = as.integer(maxit),
                      tpar = as.double(tpar),
                      tol = as.double(tol),
                      alfa1 = as.double(alf1),
                      alfa2 = as.double(alf2),
                      alpha = double(1),
                      sigma = as.double(sigma),
                      nit = integer(1),
                      c1c2 = double(2),
                      a123 = double(3),
                      PACKAGE = "robust")

    alpha <- f.res$alpha
    sigma <- f.res$sigma 
    mu <- gamma(1.0 + 1.0 / alpha) * sigma 
    zl <- list(shape = alpha, scale = sigma, mu = mu, ok = f.res$nit)

    if(vcov) {
      f.cov <- .Fortran("rlvarweb",
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
                        PACKAGE = "robust")

      vcov <- matrix(f.cov$vsiga, 2, 2)
      V.mu <- f.cov$vmoy
      dimnames(vcov) <- list(c("scale", "shape"), c("scale", "shape"))
      vcov <- vcov[2:1, 2:1]
      zl$vcov <- vcov / nobs
      zl$V.mu <- V.mu / nobs
    }
  }

  else if(estim == "tdmean") {
    u <- control$u
    beta <- control$beta
    gam <- control$gam
    vcov <- control$vcov
    tol <- control$tol
    n <- length(x)
    logx <- log(x)

    z <- .Fortran("rltmadve",
                  x = as.double(logx),
                  n = as.integer(n),
                  beta = as.double(beta),
                  gam = as.double(gam),
                  pos = double(1),
                  scal = double(1),
                  sx = double(n),
                  PACKAGE = "robust")

    Pos <- z$pos
    Scal <- z$scal

    zlw <- .Fortran("rltrmadlw",
                    alpha = as.double(1.0),
                    beta = as.double(beta),
                    gam = as.double(gam),
                    tol = as.double(tol),
                    mf = double(1),
                    sf = double(1),
                    isol = integer(1),
                    PACKAGE = "robust")

    pos.lw <- zlw$mf
    scal.lw <- zlw$sf
    v <- Scal / scal.lw
    tau <- Pos - pos.lw * v
    scale <- exp(tau)
    shape <- 1 / v
    ok <- zlw$isol
    D.shap <- shape
    D.scal <- scale

    if(ok == 0) {
      zl <- list(alpha = D.shap, sigma = D.scal, mu = NA, Tl = NA, Tu = NA,
                 ok = 0, call = the.call)
      return(zl)
    }

    Dq <- .Fortran("rlquqldw",
                   u = as.double(u),
                   alpha = as.double(D.shap),
                   sigma = as.double(D.scal),
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
      #mu <- S.tcmean.E(x, Dq$ql, Dq$qu)
      mnu <- x[x > Dq$ql && x <= Dq$qu]
      mu <- mean(mnu)
    }

    zl <- list(shape = D.shap, scale = D.scal, mu = mu, Tl = Dq$ql, Tu = Dq$qu,
               ok = Dq$isol)

    if(vcov) {
    #xmax <- limit.w(D.shap,D.scal)

      shape <- D.shap
      scale <- D.scal
      tl <- 1e-10
      mu <- gamma(1.0 + 1.0 / shape) * scale
      lim <- 5 * mu
      dw <- dweibull(lim, shape, scale)

      if(is.na(dw)) {
        lim <- mu
        mu <- 10 / shape
        dw <- dweibull(lim, shape, scale)
      }

      repeat {
        if(dw < tl)
          break

        lim <- lim + mu

        dw <- dweibull(lim, shape, scale)

        if(is.na(dw)) {
          lim <- lim - mu
          break
        }
      }

      xmax <- lim
      Theta <- S.Theta.weibull(shape, scale, u, beta, gam)

      #z <- S.quql.w(u,D.shap,1); ok <- z$ok

      z <- .Fortran("rlquqldw",
                    u = as.double(u),
                    alpha = as.double(D.shap),
                    sigma = as.double(1.0),
                    tol = as.double(tol),
                    ql = double(1),
                    qu = double(1),
                    isol = integer(1),
                    PACKAGE = "robust")

      ok <- z$isol

      itc <- 0
      if(ok != 0 || itc != 0) {
        teta  <- unlist(Theta)
        nt <- length(teta)
        til <- 1e-4

        z <- .Fortran("rlavtcmw",
                      teta = as.double(teta),
                      nt = as.integer(nt),
                      alpha = as.double(D.shap),
                      sigma = as.double(D.scal),
                      itc = as.integer(itc),
                      upper = as.double(xmax),
                      til = as.double(til),
                      sum = double(1),
                      iwork = integer(80),
                      work = double(320),
                      PACKAGE = "robust")

        zl$V.mu  <- z$sum / length(x)
      }
    } 
  }

	else
		stop("Invalid Estimator")

  estimate <- c(zl$shape, zl$scale)
  names(estimate) <- c("shape", "scale")
  sd <- if(!is.null(zl$vcov)) sqrt(diag(zl$vcov)) else c(NA, NA)

  ans <- list(estimate = estimate,
              sd = sd,
              vcov = zl$vcov,
              mu = zl$mu,
              V.mu = zl$V.mu,
              control = control,
              call = the.call,
              densfun = "weibull",
              data.name = data.name,
              x = data)

  oldClass(ans) <- "fitdstn"
  ans
}


