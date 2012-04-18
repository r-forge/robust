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

  stddev <- object$scale
  cov <- object$cov
  var <- diag(cov)

  if(p < ptotal)
    R <- R[1:p, 1:p, drop = FALSE]

  stdev.coef <- sqrt(var)

  if(correlation) {
    std <- stdev.coef %o% stdev.coef
    correl <- cov / std
  }
  else 
    correl <- NULL

  coefs <- matrix(coefs, p, 4)
  dimnames(coefs) <- list(coef.names, 
                          c("Value", "Std. Error", "t value", "Pr(>|t|)"))
  coefs[, 2] <- stdev.coef
  coefs[, 3] <- coefs[, 1] / coefs[, 2]
  coefs[, 4] <- if(rdf > 0) 2 * (1 - pt(abs(coefs[, 3]), rdf)) else NA

  if(is.null(object$robust.control))
    testbias <- TRUE

  else {
    est <- object$est
    fnl <- object$robust.control$final.alg
    if ((casefold(est) == "final") && 
        (casefold(fnl) == "mm" || casefold(fnl) == "m"))
      testbias <- TRUE
    else
      testbias <- FALSE
  }

  if(testbias)
    biasTest <- test.lmRob(object)

  if(bootstrap.se)
    bootstrap.se <- rb(object)

  int <- attr(object$terms, "intercept")
  object <- object[c("call", "terms", "iter.final.coef", "iter.refinement", 
                     "M.weights", "r.squared", "est","robust.control",
                     "genetic.control","na.action")]

  object$residuals <- res
  object$coefficients <- coefs
  object$sigma <- stddev
  if(bootstrap.se[1])
    object$bootstrap.se <- bootstrap.se
  object$df <- c(p, rdf, ptotal)
  object$cov.unscaled <- cov/stddev^2
  object$correlation <- correl

  if(testbias)
    object$biasTest <- biasTest
  oldClass(object) <- "summary.lmRob"

  object
}

