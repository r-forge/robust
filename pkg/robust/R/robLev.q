robMD2 <- function(object)
{
  m <- model.frame(object)

  if(is.null(m))
    return(rep(as.numeric(NA), length(fitted(object))))

  m.terms <- terms(m)
  X <- model.matrix(m.terms, m)

  dc <- attr(m.terms, "dataClasses")
  tl <- attr(m.terms, "term.labels")
  numeric.vars <- intersect(tl, names(dc)[dc == "numeric"])

  if(length(numeric.vars)) {
    X2 <- as.matrix(m[numeric.vars])
    mcd <- covMcd(X2)
    w <- mcd$mcd.wt
    sum.w <- sum(w)

    mu <- apply(X2, 2, weighted.mean, w = w)

    X2.tilde <- sweep(X2, 2, mu)
    X2.tilde <- sqrt(prod(mcd$cnp) * (nrow(X2.tilde) - 1)/(sum.w - 1)) * w * X2.tilde

    m[numeric.vars] <- sweep(X2.tilde, 2, mu, FUN = "+")
  }

  D <- model.matrix(m.terms, m)

  if(attr(m.terms, "intercept")) {
    X <- X[, -1, drop = FALSE]
    D <- D[, -1, drop = FALSE]
  }

  stats::mahalanobis(X, colMeans(D), var(D))
}


designMD.lmRob <- function(object, ...)
  robMD2(object)


## Methods for other packages ##


designMD.lmrob <- function(object, ...)
  robMD2(object)


designMD.glmrob <- function(object, ...)
  robMD2(object)


designMD.rlm <- function(object, ...)
  robMD2(object)








