glmRob.cubif.Ini <- function(X, y, ni, icase, offset, b, zmin, epsilon, ia1, control)
{
  # Compute distances (dist), weights (ai), initial coefficients,
  # intial ci. If an intercept is present, assume that the first
  # column of X is made of ones

  n <- length(y)
  np <- dim(as.matrix(X))[2]

  # Mahalanobis distances and weights

# if(np == 1 & all(X == 1)) {
#   dist <- rep(1, n)
#   ai <- rep(b, n)
# }

  if(np <= 2 && all(X[ , 1] == 1)) {
    dist <- rep(1, n)
    ai <- rep(b, n)
  }

  else {

    if(all(X[, 1] == 1)) {
      Z <- as.matrix(X[, -1])
      Zmcd <- covRob(Z, estim = control$estim, control = control,
        distance = FALSE)
      Minv <- solve(Zmcd$cov)
      Zc <- sweep(Z, 2, Zmcd$center)
    }

    else {
      Zc <- as.matrix(X)
      Zmcd <- covRob(Zc, estim = control$estim, control = control,
        distance = FALSE)
      mu <- as.matrix(Zmcd$center)
      Mu <- Zmcd$cov + mu %*% t(mu)
      Minv <- solve(Mu)
    }

    ai <- dist <- rep(1, n)

    for(i in 1:n) {
      z <- as.matrix(Zc[i,  ])
      dist[i] <- sqrt((t(z) %*% Minv) %*% z)
      ai[i] <- b/max(zmin, dist[i])
    }
  }

  # Initial value of theta and vartheta; set c_i=0 #

  zi <- glmRob.Initial.LMS(X, y, ni, dist, offset, icase) 

  theta0 <- zi$theta0[1:np]
  ci <- rep(0.0, n)
  vtheta <- as.vector(X %*% theta0)

  # Initial covariance matrix of estimated theta #

  z <- glmRob.gfedca(vtheta, ci, ai, ni, offset, icase)
  zc <- glmRob.ktaskw(x = X, d = z$d, e = z$e, f = 1/n, f1 = 1,
    iainv = 0, ia = ia1, tau = epsilon)

  covi <- zc$cov

  list(theta = theta0, ci = ci, cov = covi, dist = dist, ai = ai)
}


