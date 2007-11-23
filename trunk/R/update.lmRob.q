update.lmRob <- function(object, formula, evaluate = TRUE, class, ...)
{
  if(missing(formula)) {

    if(is.null(object$T.coefficients))
      stop("There is nothing to update.")

    ans <- object

    if(object$est == "initial")
      ans$est <- "final"

    else 
      ans$est <- "initial"

    ans$coefficients <- object$T.coefficients
    ans$T.coefficients <- object$coefficients
    ans$cov <- object$T.cov
    ans$T.cov <- object$cov
    ans$residuals <- object$T.residuals
    ans$T.residuals <- object$residuals
    ans$fitted.values <- object$T.fitted.values
    ans$T.fitted.values <- object$fitted.values

    if (casefold(object$robust.control$final) == "mm" ||
        casefold(object$robust.control$final) == "m") {
      ans$dev   <- object$T.dev
      ans$T.dev <- object$dev
      ans$r.squared   <- object$T.r.squared
      ans$T.r.squared <- object$r.squared
      ans$M.weights   <- object$T.M.weights
      ans$T.M.weights <- object$M.weights
    }
  }

  else
    ans <- NextMethod()

  ans
}


