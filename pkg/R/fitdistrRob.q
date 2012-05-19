fitdistrRob <- function(x, densfun, ...)
{
  the.call <- match.call()
  if(densfun == "log-normal") densfun <- "lognormal"
  densfun <- match.arg(densfun, c("gamma", "lognormal", "weibull"))

  ans <- switch(densfun,
    gamma = gammaRob(x, ...),
    lognormal = lognormRob(x, ...),
    weibull = weibullRob(x, ...)
  )

  oldClass(ans) <- "fitdistrRob"
  ans
}


