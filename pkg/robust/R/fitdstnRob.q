fitdstnRob <- function(x, densfun, ...)
{
  the.call <- match.call()

  x <- as.vector(x)

  if(densfun == "log-normal") densfun <- "lognormal"
  densfun <- match.arg(densfun, c("gamma", "lognormal", "weibull"))

  ans <- switch(densfun,
    gamma = gammaRob(x, ...),
    lognormal = lognormRob(x, ...),
    weibull = weibullRob(x, ...)
  )

  oldClass(ans) <- "fitdstnRob"
  ans
}


