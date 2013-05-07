gen.data <- function(coeff, n = 100, eps = 0.1, sig = 3,
                     snr = 1/20, seed = 837)
{

# coeff : 3 x 1 vector of coefficients
# eps   : the contamination ratio, between 0 and 0.5
# sig   : standard deviation of most observations
# snr   : signal-to-noise ratio, well, not really
# Note  : the regressors are generated as: rnorm(n,1),
#         rnorm(n,1)^3, exp(rnorm(n,1)). It also
#         generates an unused vector x4.

  set.seed(seed)
  x <- cbind(rnorm(n, 1), rnorm(n, 1)^3, exp(rnorm(n, 1)))
  ru <- runif(n)
  n1 <- sum(ru < eps)
  u <- numeric(n)
  u[ru < eps] <- rnorm(n1, sd = sig/snr)
  u[ru > eps] <- rnorm(n - n1, sd = sig)

  data.frame(y = x %*% matrix(coeff, ncol = 1) + u,
    x1 = x[,1], x2 = x[,2], x3 = x[,3], x4 = rnorm(n, 1))
}


