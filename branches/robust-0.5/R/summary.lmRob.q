## Todo: test with weighted lmRob model

summary.lmRob <- function(object, correlation = FALSE, bootstrap.se = FALSE, ...)
{
  wt <- object$M.weights
  wt1 <- object$weights

  if(!is.null(wt1) && !is.null(wt))
    wt <- wt * wt1

  coefs <- coef(object)
  coef.names <- names(coefs)
  res <- object$residuals
  fv <- object$fitted
  n <- length(res)
  p <- ptotal <- length(coefs)

  if(any(na <- is.na(coefs))) {
    coefs <- coefs[na]
    p <- length(coefs)
  }

  rdf <- object$df.residual

  if(!is.null(wt1)) {
    wt1 <- wt1^0.5
    res <- res * wt1
    fv <- fv * wt1
    excl <- wt1 == 0

    if(any(excl)) {
      warning(paste(sum(excl), "rows with zero weights not counted"))
      res <- res[!excl]
      fv <- fv[!excl]
      wt1 <- wt1[!excl]
      if(is.null(object$df.residual))
        rdf <- rdf - sum(excl)
      wt <- wt * wt1
    }
  }

  stderr.coefs <- sqrt(diag(object$cov))
  tval <- coefs / stderr.coefs
  pval <- 2.0 * pt(abs(tval), rdf, lower.tail = FALSE)

  if(is.null(object$robust.control))
    testbias <- TRUE

  else {
    est <- casefold(object$est)
    fnl <- casefold(object$robust.control$final.alg)
    testbias <- ((est == "final") && (fnl %in% c("m", "mm")))
  }

  ans <- object[c("call", "terms")]
  ans$residuals <- res
  ans$coefficients <- cbind(coefs, stderr.coefs, tval, pval)
  dimnames(ans$coefficients) <- list(coef.names, c("Estimate", "Std. Error",
                                                   "t value", "Pr(>|t|)"))
  ans$aliased <- is.na(coef(object))
  ans$sigma <- object$scale
  ans$df <- c(p, rdf, p)
  ans$r.squared <- object$r.squared
  ans$cov.unscaled <- object$cov / (object$scale)^2
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 1)]

  if(correlation) {
    ans$correlation <- object$cov / (stderr.coefs %o% stderr.coefs)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
  }

  if(!is.null(object$na.action)) 
    ans$na.action <- object$na.action

  ans$est <- object$est

  if(testbias)
    ans$biasTest <- test.lmRob(object)

  if(bootstrap.se)
    ans$bootstrap.se <- rb.lmRob(object)

  class(ans) <- "summary.lmRob"
  ans
}
