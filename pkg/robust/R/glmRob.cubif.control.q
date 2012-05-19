glmRob.cubif.control <- function(epsilon = 0.001, maxit = 50, 
  bpar = 2, cpar = 1.5, trc = FALSE, ...)
{
  #init.cov <- covRob.control(estim = "mcd", quan = .75, ...)

  ## quan (alpha) = 0.75 was already to big - we really should be using
  ## the defaults here but doing so breaks the breslow.dat example. How
  ## to handle mixed factor/continuous variables?

  init.cov <- covRob.control(estim = "mcd", quan = .85, ...)
  init.cov$estim <- "mcd"

  list(epsilon = epsilon, maxit = maxit, bpar = bpar, cpar = cpar,
    trc = trc, init.cov = init.cov) 
}

