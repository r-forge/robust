## Fortran code is at end of ../src/lmrobmm.f
##                           ~~~~~~~~~~~~~~~~

psi.weight <- function(x, ips = 1, xk = 1.06)
{
    x <- as.double(x)
    stopifnot(is.double(x), ips %in% 1:4)
    n <- length(x)# *is* integer
    .Fortran("rlpsiam2", n, x,
             fvals= double(n),
             as.integer(ips),
             as.double(xk),
             PACKAGE = "robust")$fvals
}


rho.weight <- function(x, ips = 1, xk = 1.06) {
  x <- as.double(x)
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)# *is* integer
  .Fortran("rlrhoam2", n, x,
           fvals= double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


psp.weight <- function(x, ips = 1, xk = 1.06) {
  x <- as.double(x)
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)# *is* integer
  .Fortran("rlpspam2", n, x,
           fvals= double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}


chi.weight <- function(x, ips = 1, xk = 1.06) {
  x <- as.double(x)
  stopifnot(is.double(x), ips %in% 1:4)
  n <- length(x)# *is* integer
  .Fortran("rlchiam2", n, x,
           fvals= double(n),
           as.integer(ips),
           as.double(xk),
           PACKAGE = "robust")$fvals
}

