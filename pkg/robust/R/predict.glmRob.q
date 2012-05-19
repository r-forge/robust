predict.glmRob <-
function(object, newdata, type = c("link", "response", "terms"), 
    se.fit = FALSE, terms = labels(object), dispersion = NULL, ...)
{
  type <- match.arg(type)
  save.na.action <- object$na.action
  object$na.action <- NULL
  out <- if(!se.fit) {
#No standard errors
    if(missing(newdata)) switch(type,
        link = object$linear.predictors,
        response = object$fitted,
        terms = { 
          #class(object) <- c(class(object), "lm")
          oldClass(object) <- "lm"
          NextMethod("predict")
        }
    )   
    else switch(type, 
        response =  {
          #class(object) <- c(class(object), "lm")
          oldClass(object) <- "lm"
          family(object)$inverse(NextMethod("predict"))
        }
        ,
        link = {
          type <- "response"
          #class(object) <- c(class(object), "lm")
          oldClass(object) <- "lm"
          NextMethod("predict")
        }
        ,
        {
          #class(object) <- c(class(object), "lm")
          oldClass(object) <- "lm"
          NextMethod("predict")
        } )
  }
  else {
#With standard errors
    if(is.null(dispersion) || dispersion == 0)
      dispersion <- summary(object, dispersion = dispersion)$dispersion
    switch(type,
      response = {

        Terms <- object$terms
        offset <- attr(Terms, "offset")
        intercept <- attr(Terms, "intercept")
        xbar <- NULL
        if(missing(newdata)) 
          x <- model.matrix(object)
        else if(!((is.atomic(newdata) && length(newdata) == 1
          && length(object$coef) != 1 && newdata > 0 
          && (newdata - trunc(newdata) < .Machine$single.eps))
          | is.list(newdata))) {
            if(!is.null(offset)) {
            warning("Offset not included")
            offset <- NULL
          }
          TT <- length(object$coef)
          if(is.matrix(newdata) && ncol(newdata) == TT)
            x <- newdata
          else if(length(newdata)==TT) 
            x <- matrix(x, 1, TT)
          else stop("Argument \"newdata\" cannot be coerced")
        } else {
          x <- model.matrix(delete.response(Terms),
            newdata, contrasts = object$contrasts,
            xlevels = attr(object, "xlevels"))
          if(!is.null(offset))
            offset <- eval(attr(Terms, "variables")[offset], 
              newdata)
        }
        coefs <- coef(object)
        asgn <- attr(coefs, "assign")
        nac <- is.na(objects$coef)
        if(any(nac)) {
          xbar <- xbar[!nac]
          x <- x[,!nac]
        }
        attr(x, "constant") <- xbar
        fit.summary <- summary(object)
        #pred <- Build.terms(x, coefs, ??, asgn, collapse = TRUE)
        cov <- fit.summary$cov.unscaled
        ft <- drop(x %*% coefs)
        vv <- rowSums((x %*% cov) * x)
        pred <- list(fit = ft, se.fit = drop(sqrt(vv)))
        pred$df <- object$df.resid
        if(!is.null(offset)) {
          if(missing(newdata))
            warning("Offset not included")
          else pred$fit <- pred$fit + offset
        }
        if(missing(newdata) && !is.null(object$na.action)) {
          pred$fit <- napredict(object$na.action, pred$fit)
          pred$se.fit <- napredict(object$na.action, pred$se.fit)
        }
        famob <- family(object)
        pred$fit <- famob$inverse(pred$fit)
        pred$se.fit <- (pred$se.fit * sqrt(dispersion))
        pred$residual.scale <- as.vector(sqrt(dispersion))
        pred$se.fit <- pred$se.fit/abs(famob$deriv(pred$fit))
        pred
      }
      ,
      link = {
        type <- "response"
        Terms <- object$terms
        offset <- attr(Terms, "offset")
        intercept <- attr(Terms, "intercept")
        xbar <- NULL
        if(missing(newdata)) 
          x <- model.matrix(object)
        else if(!((is.atomic(newdata) && length(newdata) == 1
          && length(object$coef) != 1 && newdata > 0 
          && (newdata - trunc(newdata) < .Machine$single.eps))
          | is.list(newdata))) {
            if(!is.null(offset)) {
            warning("Offset not included")
            offset <- NULL
          }
          TT <- length(object$coef)
          if(is.matrix(newdata) && ncol(newdata) == TT)
            x <- newdata
          else if(length(newdata)==TT) 
            x <- matrix(x, 1, TT)
          else stop("Argument \"newdata\" cannot be coerced")
        } else {
          x <- model.matrix(delete.response(Terms),
            newdata, contrasts = object$contrasts,
            xlevels = attr(object, "xlevels"))
          if(!is.null(offset))
            offset <- eval(attr(Terms, "variables")[offset], 
              newdata)
        }
        coefs <- coef(object)
        asgn <- attr(coefs, "assign")
        nac <- is.na(objects$coef)
        if(any(nac)) {
          xbar <- xbar[!nac]
          x <- x[,!nac]
        }
        attr(x, "constant") <- xbar
        fit.summary <- summary(object)
        #pred <- Build.terms(x, coefs, ??, asgn, collapse = TRUE)
        cov <- fit.summary$cov.unscaled
        ft <- drop(x %*% coefs)
        vv <- rowSums((x %*% cov) * x)
        pred <- list(fit = ft, se.fit = drop(sqrt(vv)))
        pred$df <- object$df.resid
        if(!is.null(offset)) {
          if(missing(newdata))
            warning("Offset not included")
          else pred$fit <- pred$fit + offset
        }
        if(missing(newdata) && !is.null(object$na.action)) {
          pred$fit <- napredict(object$na.action, pred$fit)
          pred$se.fit <- napredict(object$na.action, pred$se.fit)
        }
        pred
      }
      , # type = "terms"
      {
        Terms <- object$terms
        offset <- attr(Terms, "offset")
        intercept <- attr(Terms, "intercept")
        xbar <- NULL
        if(missing(newdata)) {
          x <- model.matrix(object)
          if(intercept) {
            xbar <- colMeans(x)
            x <- sweep(x, 2, xbar)
          }
        }
        else if(!((is.atomic(newdata) && length(newdata) == 1
          && length(object$coef) != 1 && newdata > 0 
          && (newdata - trunc(newdata) < .Machine$single.eps))
          | is.list(newdata))) {
            if(!is.null(offset)) {
            warning("Offset not included")
            offset <- NULL
          }
          TT <- length(object$coef)
          if(is.matrix(newdata) && ncol(newdata) == TT)
            x <- newdata
          else if(length(newdata)==TT) 
            x <- matrix(x, 1, TT)
          else stop("Argument \"newdata\" cannot be coerced")
        } else {
          x <- model.matrix(delete.response(Terms),
            newdata, contrasts = object$contrasts,
            xlevels = attr(object, "xlevels"))
          if(!is.null(offset))
            offset <- eval(attr(Terms, "variables")[offset], 
              newdata)
        }
        if(!missing(newdata) && intercept) {
          xold <- model.matrix(object)
          xbar <- colMeans(xold)
          x <- sweep(x, 2, xbar)
        }
        coefs <- coef(object)
        asgn <- attr(coefs, "assign")
        terms <- match.arg(terms, labels(object))
        asgn <- asgn[terms]
        nac <- is.na(object$coef)
        if(any(nac)) {
          xbar <- xbar[!nac]
          x <- x[, !nac]
        }
        attr(x, "constant") <- xbar
        fit.summary <- summary(object)
        #pred <- Build.terms(x, coefs, ??, asng, collapse = FALSE)
        cov <- fit.summary$cov.unscaled
        constant <- attr(x, "constant")
        if(!is.null(constant))
          constant <- sum(constant*coefs)
        assign <- asgn
        se <- fit <- array(0, c(nrow(x), length(assign)), 
          list(dimnames(x)[[1]], names(assign)))
        TL <- sapply(assign, length)
        simple <- TL == 1
        complex <- TL > 1
        if(any(simple)) {
            asss <- unlist(assign[simple])
            ones <- rep(1, nrow(x))
            fit[, simple] <- x[, asss]*outer(ones, coefs[asss])
            se[,simple] <- abs(x[,asss])*outer(ones, 
              sqrt(diag(cov))[asss])
        }
        if(any(complex)) {
          assign <- assign[complex]
          for(term in names(assign)) {
            TT <- assign[[term]]
            xt <- x[,TT]
            fit[, term] <- xt %*% coefs[TT]
            se[, term] <- sqrt(rowSums(((xt %*% cov[TT, TT]) * xt)))
          }
        }
        attr(fit, "constant") <- constant
        pred <- list(fit=fit, se.fit = se)
        pred$df <- object$df.resid
        if(missing(newdata) && !is.null(object$na.action)) {
            pred$fit <- napredict(object$na.action, pred$fit)
            pred$se.fit <- napredict(object$na.action, pred$se.fit)
        }
        pred
      }
      )
  }
  if(missing(newdata) && !is.null(save.na.action))
    if(is.list(out)) {
      out$fit <- napredict(save.na.action, out$fit)
      out$se.fit <- napredict(save.na.action, out$se.fit)
    }
    else out <- napredict(save.na.action, out)
  out
}



