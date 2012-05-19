### Asymptotic Relative Efficiency (ARE) of a GM-estimator :

lmRob.eff0 <- function(mu = 1, itype = 1, iucv = 1, iwww = 1, ialfa = 1,
                       sigmx = 1.0, upper = 10, til = 1.e-4, maxit = 150,
                       tol = 1.e-5, epmach = .Machine$double.eps,
                       uflow = .Machine$double.xmin, ta = 0, tb = 0,
                       tc = 0, ipsi = 3)
{

##--------------------------------------------------
## ipsi=3: by default, use Huber's function
##--------------------------------------------------

  index <- c(mu, iwww, iucv, ipsi, itype, 1, 0)
  tc <- c(0, 0, ta, tb, tc, epmach, uflow, upper, til)
  .Fortran("rlref0bi", ## ../src/lmrobbi.f
           as.integer(index),
           as.double(tc),
           xlcnst = as.double(-1),
           as.integer(ialfa),
           as.double(sigmx),
           as.integer(maxit),
           as.double(tol),
           nit = as.integer(0),
           alfa = as.double(0),
           beta = as.double(0),
           reff = as.double(0),
           PACKAGE = "robust")[c("nit", "alfa", "beta", "reff")]
}


