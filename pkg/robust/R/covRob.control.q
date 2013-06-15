
##' for "mcd" [ -> covMcd() ], must remain compatible with rrcov.control() :
control.mcd <- function(control) {
  stopifnot(is.list(control), any("estim" == names(control)))

  ## For backwards compatibility we support the use of quan and ntrial   ##
  ## to specify alpha and nsamp for estim = "mcd", estim = "weighted"    ##
  ## and estim = "M". Providing both quan and alpha or both ntrial and   ##
  ## nsamp will result in an error.                                      ##
  if(is.null(control$alpha) && !is.null(control$quan))
    control$alpha <- control$quan
  if(is.null(control$nsamp) && !is.null(control$ntrial))
    control$nsamp <- control$ntrial

  nm.r <- names(rrCtrl <- rrcov.control()) ## + "estim"
  nmDef <- nm.r[match(nm.r, names(control), nomatch=0L) == 0L]# those *not* yet in 'control'
  control[nmDef] <- rrCtrl[nmDef]
  control[c("estim", nm.r)]
}

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

    control[c("estim", "nresamp", "maxres", "random.sample",
              "tune", "prob", "eps")]
  }

  else if(estim == "mcd" || estim == "weighted") {
    control.mcd(control)
  }

  else if(estim == "m") {

    init.control <- control.mcd(control)
    init.control$estim <- "mcd"
    control$init.control <- init.control

    if(is.null(control$r))
      control$r <- 0.45

    if(is.null(control$arp))
      control$arp <- 0.05

    if(is.null(control$eps))
      control$eps <- 1e-03

    if(is.null(control$maxiter))
      control$maxiter <- 120

    control[c("estim", "r", "arp", "eps", "maxiter",
              "init.control")]
  }

  else
    control["estim"]
}


