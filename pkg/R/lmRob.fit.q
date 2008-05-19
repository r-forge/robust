lmRob.fit <- function(x, y, x1 = NULL, x2 = NULL, x1.idx = NULL, nrep = NULL,
							robust.control = NULL, genetic.control = NULL, ...)
{
  if(!is.numeric(x))
    stop("model matrix must be numeric")

  if(!is.numeric(y))
    stop("response must be numeric")

  fit <- lmRob.fit.compute(x2, y, x1=x1, x1.idx=x1.idx, nrep=nrep,
                           robust.control=robust.control, 
                           genetic.control=genetic.control, ...)

  if(is.null(fit)) 
    return(NULL)

  fit$contrasts <- attr(x, "contrasts")
  fit
}


