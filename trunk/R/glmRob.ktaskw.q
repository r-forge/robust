glmRob.ktaskw <- function(x, d, e, tau, ia, f, f1, iainv, a)
{
  n <- length(d)
  np <- ncol(x)
  mdx <- nrow(x)
  mdsc <- np
  ncov <- np*(np+1)/2

  if(missing(a))
    a <- double(ncov)

  s1inv <- double(ncov)
  s2 <- double(ncov)
  ainv <- double(ncov)
  cov <- double(ncov)
  sc <- matrix(double(mdsc*np), mdsc, np)

  f.res <- .Fortran("rlktasbi",
    x = as.double(x),
    d = as.double(d),
    e = as.double(e),
    n = as.integer(n),
    np = as.integer(np),
    mdx = as.integer(mdx),
    mdsc = as.integer(mdsc),
    ncov = as.integer(ncov),
    tau = as.double(tau),
    ia = as.integer(ia),
    f = as.double(f),
    f1 = as.double(f1),
    iainv = as.integer(iainv),
    a = as.double(a),
    s1inv = as.double(s1inv),
    s2 = as.double(s2),
    ainv = as.double(ainv),
    cov = as.double(cov), 
    sc = as.double(sc),
    PACKAGE = "robust")

  list(a = f.res$a, s1inv = f.res$s1inv, s2 = f.res$s2,
    ainv = f.res$ainv, cov = f.res$cov)
}



