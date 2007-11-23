lmRob.wfit <- function(x, y, w, x1=NULL, x2=NULL, x1.idx=NULL, nrep=NULL, 
                       robust.control=NULL, genetic.control=NULL, ...)
{
  if(!is.numeric(x))
    stop("model matrix must be numeric")
  if(!is.numeric(y))
    stop("response must be numeric")
  if(any(w < 0))
    stop("negative weights not allowed")
  contr <- attr(x, "contrasts")
  zero <- w == 0

  if(any(zero)) {
    pos <- !zero
    r <- f <- y
    ww <- w
    x0 <- x[zero,  , drop = FALSE]
    y0 <- y[zero]
    x2 <- x2[pos,  , drop = FALSE]
    if (!is.null(x1))
      x1 <- x1[pos,  , drop = FALSE]
    y <- y[pos]
    w <- w[pos]
  }

  w.factor <- sqrt(w)
  if(!is.null(x2))
    x2 <- x2 * w.factor
  if(!is.null(x1))
    x1 <- x1 * w.factor
  y <- y * w.factor

  fit <- lmRob.fit.compute(x2, y, x1=x1, x1.idx=x1.idx, nrep=nrep,
                           robust.control=robust.control,
                           genetic.control=genetic.control, ...)

  if(is.null(fit)) 
    return(NULL)
  fit$residuals <- fit$residuals/w.factor
  fit$fitted.values <- fit$fitted.values/w.factor

  if(any(zero)) {
    nas <- is.na(fit$coef)
    if(any(nas))
      f0 <- x0[, !nas] %*% fit$coef[!nas]
    else 
      f0 <- x0 %*% fit$coef
    r[pos] <- fit$resid
    f[pos] <- fit$fitted
    r[zero] <- y0 - f0
    f[zero] <- f0
    fit$residuals <- r
    fit$fitted.values <- f
    w <- ww
  }

  fit$weights <- w
  fit$contrasts <- contr
  fit
}


