glmRob.gfedca <- function(vtheta, ci, wa, ni, oi = 0, icase) 
{
  n <- length(vtheta)
  d <- double(n)
  e <- double(n)

  if(length(oi) == 1)
    oi <- rep(0.0, n)

  f.res <- .Fortran("rlgfedca",
    vtheta = as.double(vtheta),
    ci = as.double(ci),
    wa = as.double(wa),
    ni = as.integer(ni),
    oi = as.double(oi),
    n = as.integer(n),
    icase = as.integer(icase),
    d = as.double(d),
    e = as.double(e),
    PACKAGE = "robust")

  list(d = f.res$d, e = f.res$e)
}


