robLev <- function(object, scale = c("hii", "md2"), ...)
{
  scale <- match.arg(scale)
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

  if(scale == "hii") {
    R <- qr.R(qr(D))
    U <- backsolve(R, t(X), transpose = TRUE)
    ans <- colSums(U^2)
  }
  
  else {
    if(attr(m.terms, "intercept")) {
      X <- X[, -1, drop = FALSE]
      D <- D[, -1, drop = FALSE]
    }
    ans <- mahalanobis(X, apply(D, 2, mean), var(D))
  }

  ans
}


leverage.lmRob <- function(object, ...)
  robLev(object)


## Doesn't really belong in this package ##

leverage.lmrob <- function(object, ...)
  robLev(object)

leverage.rlm <- function(object, ...)
  robLev(object)


