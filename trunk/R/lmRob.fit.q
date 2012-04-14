lmRob.fit <- function(x, y, x1.idx = NULL, nrep = NULL, robust.control = NULL,
                      ...)
{
  if(!is.numeric(x)) stop("model matrix must be numeric")
  if(!is.numeric(y)) stop("response must be numeric")

  fit <- lmRob.fit.compute(x, y, x1.idx = x1.idx, nrep = nrep,
                           robust.control = robust.control, ...)

  if(!is.null(fit))
    fit$contrasts <- attr(x, "contrasts")

  fit
}


