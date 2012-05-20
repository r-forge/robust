hatvalues.lmRob <- function(model, ...)
{
  intercept <- attributes(model$terms)$intercept
  mm <- model.matrix(model)
  n <- nrow(mm)
  p <- ncol(mm)

  if(intercept) {
    vcov <- matrix(0.0, p, p)
    u <- covRob(mm[, -1, drop = FALSE], distance = FALSE)
    vcov[2:p, 2:p] <- u$cov
    mu <- c(1, u$center)
  }

  else {
    u <- covRob(mm, distance = FALSE)
    vcov <- u$cov
    mu <- u$center
  }

  S <- (n-1) * vcov + n * mu %*% t(mu)
  mahalanobis(mm, 0, S)
}


