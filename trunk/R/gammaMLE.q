gammaMLE <- function(x, save.data = TRUE, ...) 
{	
	the.call <- match.call()
  data.name <- deparse(substitute(x))
  data <- if(save.data) x else NULL

  start <- list(shape = mean(x)^2 / var(x), scale = var(x) / mean(x))
  mle <- fitdistr(x, dgamma, start, lower = 0.0)
  shape <- mle$estimate[1]
  scale <- mle$estimate[2]
  vcov <- mle$vcov

  mu <- shape * scale
  theta <- matrix(c(scale, shape), ncol = 1)
  V.mu <- as.vector(t(theta) %*% vcov %*% theta)

  ## Kjell thinks this should be
  ## V.mu <- (vcov[2,1]^2 + vcov[1,1]*vcov[2,2]) +
  ##         (2*vcov[2,1]*shape*scale + vcov[1,1]*scale^2 + vcov[2,2]*shape^2)
  ## The second line is what we're doing now.

  header <- "MLE gamma distribution parameter estimate"

  z <- list(estimate = mle$estimate, mu = mu, V.mu = V.mu, vcov = vcov,
            call = the.call, header = header, distribution = "gamma",
            data.name = data.name, data = data)
  oldClass(z) <- c("gammaMLE", "asmDstn")
  z
}


