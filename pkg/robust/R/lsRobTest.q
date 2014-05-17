lsRobTest <- function(object, test = c("T2", "T1"), ...)
{
  test <- match.arg(test)

  if(object$robust.control$final.alg != "mm")
    stop("MM estimate required")

  eff <- object$robust.control$efficiency

  if(is.null(object$weights))
    LS <- lm(formula(object$terms), data = object$model)
  else
    LS <- lm(formula(object$terms), data = object$model, weights = object$weights)

  if(object$robust.control$weight[2] == "optimal")
    ipsi <- 1
  else if(object$robust.control$weight[2] == "bisquare")
    ipsi <- 2
  else
    stop("only bisquare and optimal weight functions supported")

  rmm <- residuals(object)
  rls <- residuals(LS)
  rob.sigma <- object$scale
  require(robust)
  tune <- lmRob.effvy(eff, ipsi)

  rw <- object$T.M.weights
  X <- model.matrix(object)
  n <- nrow(X)
  p <- ncol(X)

  if (!is.null(object$weights)) {
  	X <- X * sqrt(object$weights)
  }

  V <- (t(rw * X) %*% X) / sum(rw) 
  V.inv <- solve(V)

  if(test == "T1") {
    d <- mean(psp.weight(rmm / rob.sigma, ips = ipsi, xk = tune))
    tau <- n * mean( (psi.weight(rmm / rob.sigma, ips = ipsi, xk = tune)/d)^2 ) / (n - p)
    mat <- (1 - eff)/n * tau * rob.sigma^2 * V.inv 
  }

  if(test == "T2") {
    d <- mean(psp.weight(rmm / rob.sigma, ips = ipsi, xk = tune))
    delta2 <- mean( (rls - (rob.sigma * psi.weight(rmm / rob.sigma, ips = ipsi, xk = tune)) / d)^2 )
    mat <- delta2 / n * V.inv
  }
  
  brob <- coef(object)
  coef.names <- names(brob)
  bls <- coef(LS)
  x <- bls - brob

  if(attributes(object$terms)$intercept) {
    brob <- brob[-1]
    bls <- bls[-1]
    x <- x[-1]
    mat <- mat[-1, -1, drop = FALSE]
    coef.names <- coef.names[-1]
  }

  se <- sqrt(diag(mat))
  uniV <- x / se
  coefs <- cbind(bls, brob, x, se, uniV, 2*pnorm(-abs(uniV)))
  dimnames(coefs) <- list(coef.names, c("LS", "Robust", "Delta", "Std. Error", "Stat", "p-value"))
  T <- drop(t(x) %*% solve(mat) %*% x)
  
  ans <- list(coefs = coefs,
              full = list(stat = T, df = length(x), p.value = 1 - pchisq(T, length(x))),
              test = test,
              efficiency = eff)

  oldClass(ans) <- "lsRobTest"
  ans
}

print.lsRobTest <- function(x, digits = 4, ...)
{
  cat("Test for least squares bias\n")
  if(x$test == "T1")
    cat("H0: normal regression error distribution\n")
  if(x$test == "T2")
    cat("H0: composite normal/non-normal regression error distribution\n")

  cat("\n")
  cat("Individual coefficient tests:\n")
  print(format(as.data.frame(x$coefs), digits = digits, ...))
  cat("\n")
  cat("Joint test for bias:\n")
  cat("Test statistic: ")
  cat(format(x$full$stat, digits = digits, ...))
  cat(" on ")
  cat(format(x$full$df, digits = digits, ...))
  cat(" DF, p-value: ")
  cat(format(x$full$p.value, digits = digits, ...))
  cat("\n")

  invisible(x)
}

















