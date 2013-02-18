lognormRob.control <- function(estim, ...)
{
  estim <- match.arg(estim, choices = c("tdmean"))
  dots <- list(...)
  dots.names <- names(dots)

	if(estim == "tdmean") {
    control <- list(alpha1 = 0.5, alpha2 = 20.5, u = 0.99, beta = 0.4,
                    gam = 0.4, tol= 1e-4, cov = TRUE)

    user.args <- intersect(dots.names, names(control))
    control[user.args] <- dots[user.args]
  }

  c(list(estim = estim), control)
}


