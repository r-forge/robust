lmRob.wfit <- function(x, y, w, x1=NULL, x2=NULL, x1.idx=NULL, nrep=NULL,
                       robust.control=NULL, genetic.control=NULL, ...)
{
  if(!is.numeric(x)) stop("model matrix must be numeric")
  if(!is.numeric(y)) stop("response must be numeric")
  if(any(w < 0)) stop("negative weights not allowed")
  contr <- attr(x, "contrasts")
  has0wgts <- any(zero <- w == 0)

  if(has0wgts) {
    pos <- !zero
    r <- f <- y
    ww <- w
    x0 <- x[zero, , drop = FALSE]
    y0 <- y[zero]
    if (!is.null(x2))
      x2 <- x2[pos, , drop = FALSE]
    if (!is.null(x1))
      x1 <- x1[pos, , drop = FALSE]
    y <- y[pos]
    w <- w[pos]
  }

  rt.w <- sqrt(w)
  if(!is.null(x2)) x2 <- x2 * rt.w
  if(!is.null(x1)) x1 <- x1 * rt.w
  y <- y * rt.w

  fit <- lmRob.fit.compute(x2, y, x1=x1, x1.idx=x1.idx, nrep=nrep,
                           robust.control=robust.control,
                           genetic.control=genetic.control, ...)
  if(is.null(fit)) return(NULL)

  fit$residuals <- fit$residuals/rt.w
  fit$fitted.values <- fit$fitted.values/rt.w

  if(has0wgts) { # zero weights, "extend back"
    nas <- is.na(fit$coef)
    f0 <- if(any(nas)) x0[, !nas] %*% fit$coef[!nas] else x0 %*% fit$coef
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


