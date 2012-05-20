weibullRob.control <- function(estim, ...)
{
  estim <- match.arg(estim, choices = c("tdmean", "M"))
  dots <- list(...)
  dots.names <- names(dots)

  if(estim == "M") {
    control <- list(maxit = 100, b1 = 1.5, b2 = 1.7, A = c(0, 0, 0),
                    tol = 1e-4, til = 1e-3, vcov = TRUE, sigma = 0.0)

    user.args <- intersect(dots.names, names(control))
    control[user.args] <- dots[user.args]
  }

  if(estim == "tdmean") {
    control <- list(alpha1 = 0.5, alpha2 = 20.5, u = 0.99, beta = 0.4,
                    gam = 0.4, vcov = TRUE, tol = 1e-4)

    user.args <- intersect(dots.names, names(control))
    control[user.args] <- dots[user.args]
  }

  control
}


