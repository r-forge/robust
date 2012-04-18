glmRob.gintac <- function(x, y, ni, oi, icase, maxtt, maxta, tolt, tola, b, c) 
{
  mdx <- nrow(x)
  n <- length(y)
  np <- ncol(x)
  mdt <- n
  ncov <- np*(np+1)/2
  nitt <- integer(1)
  nita <- integer(1)
  sigma <- double(1)
  a <- double(ncov)

  if(length(oi) == 1)
    oi <- rep(0.0, n)

  theta <- double(mdt)
  ci <- double(n)
  dist <- double(n)
  rw1 <- double(5*ncov+3*n)
  rw2 <- matrix(double(1),mdx,np)
  iw1 <- integer(np)
  dw1 <- double(2*ncov+np+n)

  f.res <- .Fortran("rlgintac",
    x = as.double(x),
    y = as.double(y),
    ni = as.integer(ni),
    oi = as.double(oi),
    mdx = as.integer(mdx),
    mdt = as.integer(mdt),
    n = as.integer(n),
    np = as.integer(np),
    ncov = as.integer(ncov),
    icase = as.integer(icase),
    maxtt = as.integer(maxtt),
    maxta = as.integer(maxta),
    tau  =  as.double(tolt),
    tolt = as.double(tolt),
    tola = as.double(tola),
    b = as.double(b),
    c = as.double(c),
    nitt = as.integer(nitt),
    nita = as.integer(nita),
    sigma = as.double(sigma),
    a = as.double(a),
    theta = as.double(theta),
    ci = as.double(ci),
    dist = as.double(dist),
    rw1 = as.double(rw1),
    rw2 = as.double(rw2),
    iw1 = as.integer(iw1),
    dw1 = as.double(dw1),
    ips = as.integer(3),
    xk = as.double(1.5),
    PACKAGE = "robust")

  list(nitt = f.res$nitt, nita = f.res$nita, sigma = f.res$sigma,
    a = f.res$a, theta = f.res$theta, ci = f.res$ci, dist = f.res$dist)
}


