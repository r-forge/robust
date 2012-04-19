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

    if(is.null(control$tune))
      control$tune <- 0.95

    if(is.null(control$prob))
      control$prob <- 0.99

    if(is.null(control$eps))
      control$eps <- 0.5

    control <- control[c("estim", "nresamp", "maxres", "random.sample",
                         "tune", "prob", "eps")]
  }

  else if(estim == "mcd" || estim == "weighted") {

  ## For backwards compatibility we support the use of quan and ntrial   ##
  ## to specify alpha and nsamp for estim = "mcd", estim = "weighted"    ##
  ## and estim = "M". Providing both quan and alpha or both ntrial and   ##
  ## nsamp will result in an error.                                      ##

    if(is.null(control$alpha))
      control$alpha <- ifelse(is.null(control$quan), 0.5, control$quan)

    if(is.null(control$nsamp))
      control$nsamp <- ifelse(is.null(control$ntrial), 500, control$ntrial)

    if(is.null(control$trace))
      control$trace <- FALSE

    if(is.null(control$use.correction))
      control$use.correction <- TRUE

    if(is.null(control$tolSolve))
      control$tolSolve <- 1e-14

    if(is.null(control$seed))
      control <- control[c("estim", "alpha", "nsamp", "trace",
                           "use.correction", "tolSolve")]
    else
      control <- control[c("estim", "alpha", "nsamp", "seed", "trace",
                           "use.correction", "tolSolve")]
  }

  else if(estim == "m") {

    if(is.null(control$alpha))
      control$alpha <- ifelse(is.null(control$quan), 0.5, control$quan)

    if(is.null(control$nsamp))
      control$nsamp <- ifelse(is.null(control$ntrial), 500, control$ntrial)

    if(is.null(control$trace))
      control$trace <- FALSE

    if(is.null(control$use.correction))
      control$use.correction <- TRUE

    if(is.null(control$tolSolve))
      control$tolSolve <- 1e-14

    if(is.null(control$seed))
      init.control <- control[c("estim", "alpha", "nsamp", "trace",
                                "use.correction", "tolSolve")]
    else
      init.control <- control[c("estim", "alpha", "nsamp", "seed", "trace",
                                "use.correction", "tolSolve")]

    init.control$estim = "mcd"
    control$init.control <- init.control

    if(is.null(control$r))
      control$r <- 0.45

    if(is.null(control$arp))
      control$arp <- 0.05

    if(is.null(control$eps))
      control$eps <- 1e-03

    if(is.null(control$maxiter))
      control$maxiter <- 120

    control <- control[c("estim", "r", "arp", "eps", "maxiter",
                          "init.control")]
  }

  else
    control <- control["estim"]

  control
}


