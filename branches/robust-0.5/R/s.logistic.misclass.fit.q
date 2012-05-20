s.logistic.misclass.fit <- function(x, y, mc.gamma, maxit, mc.trc, mc.tol, beta1)
{
  #mc.fit = misclassification fit
  # solves eq 2.4

  n <- dim(x)[1]
  p <- dim(x)[2]

  if(mc.trc)
    cat("\n")

  v <- 10 * mc.tol
  j <- 0

  # newton raphson

  while((sum(abs(v)) > mc.tol) && (j < maxit)) {

    j <- j + 1      
    beta0 <- beta1
    a <- matrix(0, p, p)
    v <- rep(0, p)

    for(i in 1:n) {
      tmp <- drop(x[i,] %*% beta0)
      tmp2 <- glmRob.misclass.w(tmp, mc.gamma) * glmRob.misclass.g(tmp, mc.gamma)
      tmp3 <- (tmp2 * x[i,]) %*% t(x[i,])
      a <- a + tmp3
      tmp4 <- glmRob.misclass.w(tmp, mc.gamma) * x[i,] * (y[i] - glmRob.misclass.G(tmp, mc.gamma))
      v <- v + tmp4
    }

    a <- -a
    beta1 <- as.vector(beta0 - solve(a) %*% v)

    if(mc.trc)
      cat(j, beta1, "\n", sep = " - ")
  }

  #fit <- list(coefficients = as.vector(beta0), iter = j,
  #  converged = sum(abs(v)) < mc.tol )

  fit <- list(coefficients = as.vector(beta1), iter = j,
    converged = sum(abs(v)) < mc.tol )

  fit
}


