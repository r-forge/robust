glmRob.misclass <- function(x, y, control, offset, null.dev, family, Terms)  
{
  the.call <- match.call()
  family.name <- family$family

  if(casefold(family.name) != "binomial") 
    stop(paste("Method mallows not implemented for ",
      family.name, " family.", sep = "")) 


  if (is.factor(y))
    y <- as.numeric(y != levels(y)[1])
  else
    y <- as.vector(y)

  if(any(y>1) || is.matrix(y)) 
    stop("Response doesn't look Bernoulli. The misclass method is only implemented for Bernoulli responses")

  mc.gamma <- control$mc.gamma
  maxit <- control$mc.maxit
  mc.trc <- control$mc.trc
  mc.tol <- control$mc.tol
  initial <- control$mc.initial
    
  if(is.null(initial))
    initial <- coef(glm.fit(x,y, family = binomial()))

  n <- dim(x)[1]
  p <- dim(x)[2]

  mc.beta <- s.logistic.misclass.fit(x = x, y = y, mc.gamma = mc.gamma, 
    maxit = maxit, mc.trc = mc.trc, mc.tol = mc.tol, beta1 = initial) 

  # now use the weights

  beta <- mc.beta$coefficients
  eta <- drop(x %*% beta)

  if(is.R())
    mu <- binomial()$linkinv(eta)
  else
    mu <- binomial()$inverse(eta)

  w <- glmRob.misclass.w(eta, mc.gamma)

  w.glm.fit <- suppressWarnings(glm.fit(x = x, y = y, family = binomial(), weights = w))

  w.glm.fit$call <- the.call
  w.glm.fit$control <- control
  w.glm.fit$prior.weights <- NULL

  if(any(offset) && attr(Terms, "intercept")) {
    if(length(Terms))
      null.deviance <- glm.fit(x[, "(Intercept)", drop = FALSE], y, w, 
              offset = offset, family = family)$deviance
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

  c(w.glm.fit, list(mc.iter = mc.beta$iter, mc.beta = mc.beta$coefficients,
    mc.weights = w, mc.converged = mc.beta$converged))
}


