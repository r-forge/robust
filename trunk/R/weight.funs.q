psi.weight <- function(svals, ips = 1, xk = 1.06) {
  n <- length(svals)
  fvals <- double(n)
  storage.mode(svals) <- "double"
  f.res <- .Fortran("rlpsiam2",
                    n=as.integer(n),
                    svals=svals,
                    fvals=fvals,
                    as.integer(ips),
                    as.double(xk),
                    PACKAGE = "robust")
  f.res$fvals
}


rho.weight <- function(svals, ips = 1, xk = 1.06) {
  n <- length(svals)
  fvals <- double(n)
  storage.mode(svals) <- "double"
  f.res <- .Fortran("rlrhoam2",
                    n=as.integer(n),
                    svals=svals,
                    fvals=fvals,
                    as.integer(ips),
                    as.double(xk),
                    PACKAGE = "robust")
  f.res$fvals
}


psp.weight <- function(svals, ips = 1, xk = 1.06) {
  n <- length(svals)
  fvals <- double(n)
  storage.mode(svals) <- "double"
  f.res <- .Fortran("rlpspam2",
                    n=as.integer(n),
                    svals=svals,
                    fvals=fvals,
                    as.integer(ips),
                    as.double(xk),
                    PACKAGE = "robust")
  f.res$fvals
}


chi.weight <- function(svals, ips = 1, xk = 1.06) {
  n <- length(svals)
  fvals <- double(n)
  storage.mode(svals) <- "double"
  f.res <- .Fortran("rlchiam2",
                    n=as.integer(n),
                    svals=svals,
                    fvals=fvals,
                    as.integer(ips),
                    as.double(xk),
                    PACKAGE = "robust")
  f.res$fvals
}

