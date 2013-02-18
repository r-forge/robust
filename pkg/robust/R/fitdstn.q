fitdstn <- function(x, densfun, ...)
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
    gamma = {
      m <- mean(x)
      v <- var(x)
      fitdistr(x, dgamma, start = list(shape = m^2/v, scale = v/m), lower = 0.0)
    },

    lnorm = fitdistr(x, "lognormal"),

    weibull = fitdistr(x, "weibull", lower = 0.0)
  )

  ans <- c(ans,
           call = the.call,
           densfun = densfun,
           data.name = data.name,
           list(x = x))

  oldClass(ans) <- "fitdstn"
  ans
}


