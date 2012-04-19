residuals.glmRob <- function(object, type = c("deviance", 
  "pearson", "working", "response"), ...)
{
  type <- match.arg(type)
  mu <- object$fitted
  y <- object$y
  family <- family(object)
  switch(type,
    working = object$y - object$ci - object$fitted.values,
    pearson = (y - mu)/sqrt(family$variance(mu)),
    deviance = {
      w <- object$prior.w
      if(is.null(w))
        w <- rep(1, length(mu))
      sign(y - mu) * sqrt(family$dev.resids(y, mu, w))
      },
    response = object$y - object$fitted
  )
}


