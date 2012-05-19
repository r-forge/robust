lmRob.wm <- function(x, y, coeff0, ucov0, scale0, itype = 1, isigma = -1,
                     ipsi = 1, xk = 0.9440982, beta = 0.2661, wgt = y,
                     tlo = 0.0001, tua = 1.5e-06, mxr = 50)
{
  ##
  ## W-algorithm for robust M-estimation
  ##

  n <- length(y)
  p <- ncol(x)
  sx <- matrix(0.0, n, p)
  ncov <- length(ucov0)
  storage.mode(x) <- "double"
  storage.mode(y) <- "double"

  rList <- .Fortran("rlrwagm2",
                    x,
                    y,
                    theta = coeff0,
                    wgt = y,
                    cov = as.double(ucov0),
                    psp0 = as.double(psp.weight(0, ipsi, xk)),
                    sigmai = as.double(scale0),
                    as.integer(n),
                    as.integer(p),
                    mdx = as.integer(n),
                    as.integer(ncov),
                    tol = as.double(tlo),
                    gam = as.double(1.0),
                    tau = as.double(tua),
                    itype = as.integer(1),
                    as.integer(isigma),
                    icnv = as.integer(1),
                    maxit = as.integer(mxr),
                    maxis = as.integer(1),
                    nit = integer(1),
                    sigmaf = double(1),
                    rs = double(n),
                    delta = double(p),
                    double(n),
                    double(p),
                    double(p),
                    double(p),
                    ip = integer(p),
                    double(n),
                    sx,
                    as.integer(ipsi),
                    as.double(xk),
                    as.double(beta),
                    as.double(1.0),
                    PACKAGE = "robust")
  rList
}


