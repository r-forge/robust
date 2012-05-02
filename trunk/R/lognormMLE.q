lognormMLE <- function(x, save.data = TRUE)
{
	the.call <- match.call()
  data.name <- deparse(substitute(x))
  data <- if(save.data) x else NULL

  mle <- fitdistr(x, "lognormal")
  meanlog <- mle$estimate[1]
  sdlog <- mle$estimate[2]
  vcov <- mle$vcov

  mu <- exp(meanlog + sdlog^2 / 2)
  V.mu <- (meanlog^2 * sdlog^2 * (1 + sdlog^2 / 2)) / (length(x) - 1)

  header <- "Maximum likelihood lognormal distribution parameter estimate"

  z <- list(meanlog = meanlog, sdlog = sdlog, mu = mu, V.mu = V.mu, vcov = vcov,
            call = the.call, header = header, distribution = "lognormal",
            parameter.names = c("meanlog", "sdlog"), data.name = data.name,
            data = data)
  oldClass(z) <- c("lognormMLE", "asmDstn")
  z
}


