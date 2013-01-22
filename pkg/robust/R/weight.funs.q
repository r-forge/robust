## Fortran code is at end of ../src/lmrobmm.f
##                           ~~~~~~~~~~~~~~~~

psi.weight <- function(x, ips = 1, xk = 1.06)
{
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)

  .Fortran("rlpsiam2",
           as.integer(n),
           as.double(x),
           fvals = double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


rho.weight <- function(x, ips = 1, xk = 1.06)
{
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)

  .Fortran("rlrhoam2",
           as.integer(n),
           as.double(x),
           fvals = double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


psp.weight <- function(x, ips = 1, xk = 1.06)
{
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)

  .Fortran("rlpspam2",
           as.integer(n),
           as.double(x),
           fvals = double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


chi.weight <- function(x, ips = 1, xk = 1.06)
{
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)

  .Fortran("rlchiam2",
           as.integer(n),
           as.double(x),
           fvals = double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


