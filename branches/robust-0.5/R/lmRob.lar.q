lmRob.lar <- function(x, y, tol=1e-6)
{
  ## LAR : Least Absolute Residuals -- i.e. L_1  M-estimate

  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  storage.mode(x) <- "double"
  storage.mode(y) <- "double"
  bet0 <- 0.773372647623  ## bet0 = pnorm(0.75)
  #tmpn <- double(n)
  #tmpp <- double(p)

  z1 <- .Fortran("rllarsbi",   ##-> ../src/lmrobbi.f
                 x,
                 y,
                 as.integer(n),
                 as.integer(p),
                 as.integer(n),
                 as.integer(n),
                 as.double(tol),
                 NIT = integer(1),
                 K = integer(1),
                 KODE = integer(1),
                 SIGMA = double(1),
                 THETA = double(n),
                 RS = double(n),
                 SC1 = double(n),
                 SC2 = double(p),
                 SC3 = double(p),
                 SC4 = double(p),
                 BET0 = as.double(bet0),
                 PACKAGE = "robust")[c("THETA", "SIGMA", "RS", "NIT")]

  names(z1) <- c("coef", "scale", "resid", "iter")
  length(z1$coef) <- p

  z1
}


  ##           c("THETA","SIGMA", "RS",  "NIT")
  ##list(coef=z1$THETA[1:p], scale=z1$SIGMA, resid=z1$RS)
