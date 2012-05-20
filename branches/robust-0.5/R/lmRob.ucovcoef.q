lmRob.ucovcoef <- function(x, y, resid, sigma, p, ipsi, xk, tau, tl)
{

  ##
  ## Unscaled covariance matrix of coefficients
  ##

  wi <- resid / sigma
  wcnd <- wi != 0 & !is.na(wi)
  wi[wcnd] <- psi.weight(wi[wcnd], ipsi, xk) / wi[wcnd]
  swi <- sum(wi[wcnd])

  if (abs(swi) <= tl) {
    ans <- list(wi = wi, cov = NA, fact = NA, ierr = 1)
    return(ans)
  }

  z  <- as.vector(sqrt(wi))
  sx <- x * z
  np <- ncol(sx)
  n <- length(resid)

  storage.mode(sigma) <- "double"
  fact <- .Fortran("rlkffam2",
                   as.double(resid),
                   as.integer(n),
                   as.integer(p),
                   sigma,
                   fh = double(1),
                   as.integer(ipsi),
                   as.double(xk),
                   PACKAGE = "robust")$fh * swi

  storage.mode(sx) <- "double"
  zz <- .Fortran("rlrmtrm2",
                 x = sx,
                 as.integer(n),
                 np = as.integer(np),
                 as.integer(n),
                 intch = as.integer(1),
                 as.double(tau),
                 k = integer(1),
                 sf = double(np),
                 sg = double(np),
                 sh = double(np),
                 ip = integer(np),
                 PACKAGE = "robust")

  k <- zz$k
  xt <- zz$x
  ncov <- np*(np+1)/2

  zc <- .Fortran("rlkiasm2",
                 xt,
                 k = as.integer(k),
                 as.integer(np),
                 as.integer(n),
                 as.integer(ncov),
                 fu = as.double(1.0),
                 fb = as.double(1.0),
                 cov = double(ncov),
                 PACKAGE = "robust")$cov

  storage.mode(fact) <- "double"
  cov <- .Fortran("rlkfasm2",
                  xt,
                  cov = zc,
                  k = as.integer(k),
                  as.integer(np),
                  as.integer(n),
                  as.integer(ncov),
                  f = fact,
                  double(np),
                  zz$sg,
                  zz$ip,
                  PACKAGE = "robust")$cov

  attributes(wi) <- attributes(y)
  list(wi = wi, ucov = cov, fact = fact, ierr = 0)
}

