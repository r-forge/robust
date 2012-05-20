glmRob.gicstp <- function(icase, ialg, ni, vtheta, ai, oi, tol, maxit)
{
  n <- length(vtheta)

  if(length(oi) == 1)
    oi <- rep(0.0, n)

  ci <- double(n)

  f.res <- .Fortran("rlgicstp",
    icase = as.integer(icase),
    ialg = as.integer(ialg),
    nn = as.integer(ni),
    vtheta = as.double(vtheta),
    wa = as.double(ai),
    oi = as.double(oi),
    n = as.integer(n),
    tol = as.double(tol),
    maxit = as.integer(maxit),
    c = as.double(ci),
    PACKAGE = "robust")

  list(ci = f.res$c)
}

