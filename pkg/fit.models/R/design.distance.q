design.distance <- function(object, ...)
  UseMethod("design.distance")


design.distance.default <- function(object, ...)
{
  m <- model.frame(object)

  if(is.null(m))
    return(rep(as.numeric(NA), length(fitted(object))))

  m.terms <- terms(m)
  attr(m.terms, "intercept") <- 1
  X <- model.matrix(m.terms, m)[, -1, drop = FALSE]

  if(dim(X)[2])
    sqrt(mahalanobis(X, center = apply(X, 2, mean), cov = var(X)))
  else
    rep(0.0, length(fitted(object)))
}


