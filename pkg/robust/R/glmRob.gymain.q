glmRob.gymain <- function(x, y, ni, cov, a, theta, oi = 0, b,
  gam, tau, icase, iugl, iopt, ialg, icnvt, icnva, maxit, maxtt,
  maxta, maxtc, tol, tolt, tola, tolc, trc = FALSE) 
{
  mdx <- nrow(x)
  n <- length(y)
  np <- ncol(x)
  ncov <- length(cov)

  if (length(oi) == 1)
    oi <- rep(0.0, n)

  if(missing(a))
    a <- double(ncov)

  if(missing(theta))
    theta <- double(np)

  nit <- integer(1)
  ci <- double(n)
  wa <- double(n)
  vtheta <- double(n)
  delta <- double(np)
  grad <- double(np)
  hessnv <- double(ncov)
  rw1 <- double(5*ncov+3*n)
  rw2 <- matrix(double(mdx*np), mdx, np)
  iw1 <- integer(np)
  dw1 <- double(2*ncov+np+n)

  if(trc)
    trc <- 1
  else
    trc <- 0

  f.res <- .Fortran("rlgymain",
    x = as.double(x),
    y = as.double(y),
    ni = as.integer(ni),
    cov = as.double(cov),
    a = as.double(a),
    theta = as.double(theta),
    oi = as.double(oi),
    mdx = as.integer(mdx),
    n = as.integer(n),
    np = as.integer(np),
    ncov = as.integer(ncov),
    b = as.double(b),
    gam = as.double(gam),
    tau = as.double(tau),
    icase = as.integer(icase),
    iugl = as.integer(iugl),
    iopt = as.integer(iopt),
    ialg = as.integer(ialg),
    icnvt = as.integer(icnvt),
    icnva = as.integer(icnva),
    maxit = as.integer(maxit),
    maxtt = as.integer(maxtt),
    maxta = as.integer(maxta),
    maxtc = as.integer(maxtc),
    tol = as.double(tol),
    tolt = as.double(tolt),
    tola = as.double(tola),
    tolc = as.double(tolc),
    nit = as.integer(nit),
    ci = as.double(ci),
    wa = as.double(wa),
    vtheta = as.double(vtheta),
    delta = as.double(delta),
    grad = as.double(grad),
    hessnv = as.double(hessnv),
    rw1 = as.double(rw1),
    rw2 = as.double(rw2),
    iw1 = as.integer(iw1),
    dw1 = as.double(dw1),
    trc = as.integer(trc),
    PACKAGE = "robust")

  list(a = f.res$a, theta = f.res$theta, nit = f.res$nit,
    ci = f.res$ci, wa = f.res$wa, vtheta=f.res$vtheta,
    delta = f.res$delta, grad = f.res$grad, hessnv = f.res$hessnv,
    rw1 = f.res$rw1, tol = tol)
}

