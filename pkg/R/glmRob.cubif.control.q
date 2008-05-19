glmRob.cubif.control <- function(epsilon = 0.001, maxit = 50, 
  bpar = 2, cpar = 1.5, trc = FALSE, ...)
{
  init.cov <- covRob.control(estim = "mcd", quan = .75, ...)
  init.cov$estim <- "mcd"

  list(epsilon = epsilon, maxit = maxit, bpar = bpar, cpar = cpar,
    trc = trc, init.cov = init.cov) 
}

