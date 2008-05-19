covRob.control <- function(estim, ...)
{
	estim <- casefold(estim)
  control <- list(...)
  control$estim <- estim

	if(estim == "donostah") {

    if(is.null(control$nresamp))
      control$nresamp <- "auto"

    if(is.null(control$maxres))
      control$maxres <- "auto"

    if(is.null(control$random.sample))
      control$random.sample <- FALSE

    if(is.null(control$center))
      control$center <- TRUE

    if(is.null(control$tune))
      control$tune <- 0.95

    if(is.null(control$prob))
      control$prob <- 0.99

    if(is.null(control$eps))
      control$eps <- 0.5

    control <- control[c("estim", "nresamp", "maxres", "random.sample",
                         "center", "tune", "prob", "eps")]
  }

	else if(estim == "mcd" || estim == "weighted") {

    if(is.null(control$quan))
      control$quan <- 0.5

    if(is.null(control$ntrial))
      control$ntrial <- 500

		control <- control[c("estim", "quan", "ntrial")]
  }

	else if(estim == "m") {

    if(is.null(control$quan))
      control$quan <- 0.5

    if(is.null(control$ntrial))
      control$ntrial <- 500

    if(is.null(control$r))
      control$r <- 0.45

    if(is.null(control$alpha))
      control$alpha <- 0.05

    if(is.null(control$tau))
      control$tau <- 1e-06

    if(is.null(control$tol))
      control$tol <- 1e-03

    if(is.null(control$maxit))
      control$maxit <- 150

		control <- control[c("estim", "quan", "ntrial", "r", "alpha", "tau",
                          "tol", "maxit")]
  }

	else
		control <- control["estim"]

	control
}


