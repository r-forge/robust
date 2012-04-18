glmRob.gytstp <- function(x, y, ci, theta, wa, cov, ni, offset, tol, icase, maxit)
{

  gam <- 1
  tau <- tol
  iopt <- 1
  icnv <- 1
  nitmon <- 0

  n <- length(y)
  np <- ncol(x)
  mdx <- nrow(x)
  ncov <- length(cov)

  if(length(offset) == 1)
    offset <- rep(0.0, n)

  nit <- integer(1)
  q0 <- double(1)
  delta <- double(np)
  f0 <- double(n)
  f1 <- double(n)
  f2 <- double(n)
  vtheta <- double(n)
  grad <- double(np)
  hessnv <- double(ncov)
  rw1 <- double(5 * np)
  rw2 <- matrix(double(mdx*np), mdx, np)
  iw1 <- integer(np)

  f.res <- .Fortran("rlgytstp",
    x = as.double(x),
    y = as.double(y),
    ci = as.double(ci),
    theta = as.double(theta),
    wa = as.double(wa),
    cov = as.double(cov),
    ni = as.integer(ni),
    oi = as.double(offset),
    n = as.integer(n),
    np = as.integer(np),
    mdx = as.integer(mdx),
    ncov = as.integer(ncov),
    gam = as.double(gam),
    tol = as.double(tol),
    tau = as.double(tau),
    iopt = as.integer(iopt),
    icase = as.integer(icase),
    icnv = as.integer(icnv),
    maxit = as.integer(maxit),
    nitmon = as.integer(nitmon),
    nit = as.integer(nit),
    q0 = as.double(q0),
    delta = as.double(delta),
    f0 = as.double(f0),
    f1 = as.double(f1),
    f2 = as.double(f2),
    vtheta = as.double(vtheta),
    grad = as.double(grad),
    hessnv = as.double(hessnv),
    rw1 = as.double(rw1),
    rw2 = as.double(rw2),
    iw1 = as.integer(iw1),
    PACKAGE = "robust")

  list(theta = f.res$theta, nit = f.res$nit, q0 = f.res$q0,
    delta = f.res$delta, f0 = f.res$f0, f1 = f.res$f1,
    f2 = f.res$f2, vtheta = f.res$vtheta, grad = f.res$grad,
    hessnv = f.res$hessnv)
}

