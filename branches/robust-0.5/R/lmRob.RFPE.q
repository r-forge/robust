lmRob.RFPE <- function(object, scale = NULL)
{
  if(object$est == "initial")
    warning("Inference based on initial estimates is not recommended.")

  if(casefold(object$robust.control$final.alg) != "mm")
    stop("RFPE is only available for final MM-estimates.")

  p <- length(object$coef)

  if(is.null(scale))
    scale <- object$scale

  res <- residuals(object) / scale
  psif <- object$robust.control$weight
  efficiency <- object$robust.control$efficiency

  if(casefold(psif[2]) == "optimal")
    ipsi <- 1
  else
    ipsi <- 2

  yc <- object$yc
  a <- sum(rho.weight(res, ipsi, yc))
  b <- p*sum(psi.weight(res, ipsi, yc)^2)
  d <- sum(psp.weight(res, ipsi, yc))

  if(d <= 0.0)
    return(NA)

  a + b/d
}


