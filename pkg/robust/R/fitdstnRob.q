fitdstnRob <- function(x, densfun, ...)
{
  the.call <- match.call()
  data.name <- deparse(substitute(x))
  x <- as.numeric(x)

  if(any(x < 0)) 
    stop("input values must be >= 0")

  densfun <- match.arg(densfun, choices = c("gamma", "lnorm", "lognormal",
                                            "log-normal", "weibull"))

  if(densfun %in% c("lognormal", "log-normal"))
    densfun <- "lnorm"

  ans <- switch(densfun,
    gamma = gammaRob(x, ...),
    lnorm = lognormRob(x, ...),
    weibull = weibullRob(x, ...)
  )

  ans$call <- the.call
  ans$data.name <- data.name

  oldClass(ans) <- "fitdstn"
  ans
}


