glmRob.mallows <- function(x, y, control, offset, null.dev, family, Terms)  
{
  the.call <- match.call()
  family.name <- family$family

  if(casefold(family.name) != "binomial") 
    stop(paste("The mallows method is not implemented for the ", 
      family.name, " family.", sep = "")) 

  if(is.factor(y))
    y <- as.numeric(y != levels(y)[1])
  else
    y <- as.vector(y)

  if(any(y > 1) || is.matrix(y)) 
    stop("Response doesn't look Bernoulli. The mallows method is only implemented for Bernoulli responses.")

  wt.fn <- control$wt.fn
  b <- control$wt.tuning
      
  x0 <- x
  tmp <- dimnames(x0)[[2]] == "(Intercept)"

  if(any(tmp)) x0 <- x0[,!tmp, drop = FALSE]

  n <- dim(x0)[1]
  p <- dim(x0)[2]

  #tmp <- fastmcd(x0, print.it = FALSE)
  tmp <- covMcd(x0, use.correction = FALSE)
  mu <- tmp$center
  V <- tmp$cov
  x1 <- scale(x0, center = mu, scale = rep(1,p))
      
  #V <- var(x0)
  #x1 <- scale(x0, scale = rep(1,p))

  tmp2 <- solve(V) %*% t(x1)
  d1 <- diag(x1 %*% tmp2)

  d1 <- sqrt(d1 / p)
  w <- wt.fn(d1, b)

  # now use the weights

  #w.glm.fit <- glm.fit(x = x, y = y, family = binomial(), 
  #  offset = offset, w = w, null.dev = TRUE)

  old.warn <- options()$warn
  on.exit(options(warn = old.warn))
  options(warn = -1)

  w.glm.fit <- glm.fit(x = x, y = y, weights = w, offset = offset,
                       family = binomial())
  w.glm.fit$call <- the.call
  w.glm.fit$control <- control
  w.glm.fit$prior.weights <- NULL

  ## we need null.deviance here!!!

  if(any(offset) && attr(Terms, "intercept")) {
    if(length(Terms)) {
      null.deviance <- glm.fit(x[, "(Intercept)", drop = FALSE],
        y, w, offset = offset, family = family)$deviance
    }
    else
      null.deviance <- w.glm.fit$deviance

    w.glm.fit$null.deviance <- null.deviance
  }

  # cov matrix

  p <- dim(x)[2]
  n <- dim(x)[1]

  tmp1 <- tmp2 <- matrix(0, p, p)
  tmp3 <- x %*% w.glm.fit$coef

  for(i in 1:n) {
    tmp <- x[i,] %*% t(x[i,]) 
    tmp <- tmp * glmRob.misclass.f( tmp3[i] )
    tmp1 <- tmp1 + tmp * w[i]
    tmp2 <- tmp2 + tmp * w[i] * w[i]
  }

  tmp1 <- tmp1 / n
  tmp2 <- tmp2 / n

  cov <- solve(tmp1) %*% tmp2 %*% solve(tmp1)
  cov <- cov / n

  xn <- dimnames(x)[[2]]

  dimnames(cov) <- list(xn, xn)
  w.glm.fit$cov <- cov

  c(w.glm.fit, list(mallows.weights = w))
}



