lognormMLE <- function(data, save.data = TRUE)
{
  # Estimate parameters of a lognormal distribution.
  # Take a log transform and estimate parameters for that normal distn

  y <- log(data)
  meany <- mean(y)
  sigma2 <- var(y)

  # Calculate mu = estimated mean of the lognormal distn

  mu <- exp(meany + 0.5 * sigma2)

  # V.mu is the variance of estimate "mu"

	V.mu <- (mu^2 * sigma2 * (1 + sigma2/2)) / (length(y) - 1)

  ans <- list(call = match.call(),
              alpha = meany,
              sigma = sqrt(sigma2),
              mu = mu,
              n = length(data),
              V.mu = V.mu)

  if(save.data)
    ans$data <- data

	ans$header <- "MLE lognormal distribution parameter estimate"
	ans$plot.header <- "MLE Estimate of Lognormal Density"
	ans$density.fn <- dlnorm
	ans$quantile.fn <- qlnorm
  oldClass(ans) <- c("lognormMLE", "asmDstn")
  ans
}



