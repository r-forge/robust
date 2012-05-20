summary.glmRob <- function(object, correlation = TRUE, ...)
{
  coef <- object$coef
  resid <- object$residuals
  dresid <- residuals(object, "deviance")
  n <- length(resid)
  p <- object$rank

  if(is.null(p))
    p <- sum(!is.na(coef))

  if(!p) {
    warning("This model has zero rank --- no summary is provided")
    return(object)
  }

  nsingular <- length(coef) - p
  rdf <- object$df.resid

  if(is.null(rdf))
    rdf <- n - p

  family.name <- family(object)$family
  dispersion <- 1
  names(dispersion) <- family.name

  covun <- object$cov
  var <- diag(covun)
  nas <- is.na(coef)
  cnames <- names(coef[!nas])
  coef <- matrix(rep(coef[!nas], 4), ncol = 4)
  dimnames(coef) <- list(cnames, c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  coef[, 2] <- sqrt(var)
  coef[, 3] <- coef[, 1]/coef[, 2]
  coef[, 4] <- 2 * pnorm(-abs(coef[, 3]))

  if(correlation) {
    cor <- covun

    for(i in 1:nrow(cor)) {

      if (var[i]<1.e-10) {
        str <- paste("Variance number",i,"smaller than 1.e-10",
          "(set to 1.e-10)")
        print(str)
      }

      cor[i,1:i] <- cor[i,1:i]/sqrt(var[i]*var[1:i])
      cor[1:i,i] <- cor[i,1:i]
    }

    dimnames(cor) <- list(cnames, cnames)
  }

  else
    cor <- NULL

  dimnames(covun) <- list(cnames, cnames)
  ocall <- object$call

  if(!is.null(form <- object$formula)) {
    if(is.null(ocall$formula))
      ocall <- match.call(get("glm"), ocall)
    ocall$formula <- form
  }

  ans <- list(call = ocall, terms = object$terms, coefficients = coef,
    dispersion = dispersion, df = c(p, rdf), deviance.resid = dresid,
    cov.unscaled = covun, correlation = cor, deviance = deviance(object),
    null.deviance = object$null.deviance, iter = object$iter, nas=nas)

  oldClass(ans) <- "summary.glmRob"

  ans
}



