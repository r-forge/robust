predict.lmRob <- function(object, newdata, type = "response", 
   se.fit = FALSE, terms = labels(object), ...)
{
  if(casefold(type) != "response")
    stop("presently only the prediction of the response is supported")

  bt <- function(x, coefs, cov = NULL, assign, collapse = TRUE)
  {
    cov.true <- !is.null(cov)

    if(collapse) {
      fit <- drop(x %*% coefs)
      if(cov.true) {
        var <- ((x %*% cov) * x) %*% rep(1.0, length(coefs))
        list(fit = fit, se.fit = drop(sqrt(var)))
      }
      else fit
    }

    else {
      constant <- attr(x, "constant")
      if(!is.null(constant))
        constant <- sum(constant * coefs)

      if(missing(assign))
        assign <- attr(x, "assign")

      if(is.null(assign))
        stop("Need an 'assign' list")

      fit <- array(0.0, c(nrow(x), length(assign)), list(dimnames(x)[[1]], names(assign)))

      if(cov.true)
        se <- fit

      TL <- sapply(assign, length)
      simple <- TL == 1
      complex <- TL > 1

      if(any(simple)) {
        asss <- unlist(assign[simple])
        ones <- rep(1.0 , nrow(x))
        fit[, simple] <- x[, asss] * outer(ones, coefs[asss])
        if(cov.true)
          se[, simple] <- abs(x[, asss]) * outer(ones, sqrt(diag(cov))[asss])
      }

      if(any(complex)) {
        assign <- assign[complex]
        for(term in names(assign)) {
          TT <- assign[[term]]
          xt <- x[, TT]
          fit[, term] <- xt %*% coefs[TT]
          if(cov.true)
            se[, term] <- sqrt(drop(((xt %*% cov[TT, TT]) * xt) %*% rep(1.0, length(TT))))
        }
      }

      attr(fit, "constant") <- constant
      if(is.null(cov))
        fit
      else
        list(fit = fit, se.fit = se)
    }
  }

  type <- match.arg(type)

  if(missing(newdata) && type != "terms" && !se.fit)
    return(fitted(object))

  Terms <- object$terms

  if(!inherits(Terms, "terms"))
    stop("invalid terms component of object")

  offset <- attr(Terms, "offset")
  intercept <- attr(Terms, "intercept")
  xbar <- NULL

  if(missing(newdata)) {
    x <- model.matrix(object)
    #center x if terms are to be computed
    if(type == "terms" && intercept) {
      xbar <- colMeans(x)
      x <- sweep(x, 2, xbar)
    }
  }

  else if(!((is.atomic(newdata) && length(newdata) == 1 && 
             length(object$coef) != 1 && newdata > 0 && 
             (newdata - trunc(newdata) < .Machine$single.eps)) | 
             is.list(newdata))) {

    #try and coerce newdata to look like the x matrix
    if(!is.null(offset)) {
      warning("Offset not included")
      offset <- NULL
    }

    TT <- length(object$coef)

    if(is.matrix(newdata) && ncol(newdata) == TT)
      x <- newdata

    else if(length(newdata) == TT)
      x <- matrix(newdata, 1, TT)

    else 
      stop("Argument \"newdata\" is not a data frame, and cannot be coerced to an appropriate model matrix.")
  }

  else {
    #newdata is a list, data frame or frame number
    x <- model.matrix(delete.response(Terms), newdata, contrasts = 
                      object$contrasts, xlevels = attr(object, "xlevels"))

    if(!is.null(offset))
      offset <- eval(attr(Terms, "variables")[offset], newdata)
  }

  if(!missing(newdata) && type == "terms" && intercept) {
    #need to center x 
    xold <- model.matrix(object)
    xbar <- colMeans(xold)
    x <- sweep(x, 2, xbar)
  }

  coefs <- coef(object)
  term.labels <- attr(Terms, "term.labels")
  asgn <- attr(coefs, "assign")
  if(is.null(asgn))
    asgn <- object$assign

  if(min(asgn) == 0) {
    nasgn <- c("(Intercept)", term.labels)
    asgn <- asgn + 1
  }
  else
    nasgn <- term.labels

  asgn.list <- list()
  n <- length(nasgn)
  for(i in 1:n)
    asgn.list[[i]] <- which(asgn == i)
  names(asgn.list) <- nasgn

  if(type == "terms") {
    terms <- match.arg(terms, labels(object))
    asgn.list <- asgn.list[terms]
  }

  nac <- is.na(object$coef)

  if(any(nac)) {
    xbar <- xbar[!nac]
    x <- x[, !nac]
  }

  attr(x, "constant") <- xbar

  if(se.fit) {
    fit.summary <- summary.lmRob(object)

    pred <- bt(x, coefs, fit.summary$cov * fit.summary$sigma^2, 
               asgn.list, collapse = type != "terms")

    pred$residual.scale <- fit.summary$sigma
    pred$df <- object$df.resid
  }

  else 
    pred <- bt(x, coefs, NULL, assign = asgn.list, 
               collapse = type != "terms")

  if(!is.null(offset) && type != "terms") {
    if(missing(newdata))
      warning("Offset not included")
    else {
      if(se.fit)
        pred$fit <- pred$fit + offset
      else 
        pred <- pred + offset
    }
  }

  if(missing(newdata) && !is.null(object$na.action)) {
    if(!se.fit)
      pred <- napredict(object$na.action, pred)
    else {
      pred$fit <- napredict(object$na.action, pred$fit)
      pred$se.fit <- napredict(object$na.action, pred$se.fit)
    }
  }
  pred
}


