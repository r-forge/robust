glmRob.glmdev <- function(y, ni, ci, wa, vtheta, offset = 0, icase = 0)
{
  n <- length(y)
  dev <- double(1)
  thetas <- double(n)
  li  <- double(n)      
  sc  <- double(n)  

  f.res <- .Fortran("rlglmdev",
    y = as.double(y),
    ni = as.integer(ni),
    ci = as.double(ci),
    wa = as.double(wa),
    vtheta = as.double(vtheta),
    offset = as.double(offset),
    n = as.integer(n),
    icase = as.integer(icase),
    dev = as.double(dev),
    thetas = as.double(thetas),
    li = as.double(li),
    sc = as.double(sc),
    PACKAGE = "robust")  

  sc <- f.res$sc 

  list(dev = f.res$dev, thetas = f.res$thetas, li = f.res$li, 
    sc = f.res$sc)
}


