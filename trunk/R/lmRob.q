#----------------------------------------------------------------#
# Robust Linear Regression                                       #
# Author: Jeffrey Wang & Kjell Konis                             #
# Date:   02/06/2002                                             #
# Insightful Corporation                                         #
#----------------------------------------------------------------#

lmRob <- function(formula, data, weights, subset,
		na.action, model = TRUE, x = FALSE, y = FALSE, contrasts = NULL,
		nrep = NULL, control = lmRob.control(...),
		genetic.control = NULL, ...)
{
  the.call <- match.call()
  m <- match.call(expand = FALSE)
  m$model <- m$x <- m$y <- m$contrasts <- m$nrep <- NULL
  m$control <- m$genetic.control <- m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, sys.parent())
  Terms <- attr(m, "terms")
  weights <- model.extract(m, weights)
  Y <- model.extract(m, "response")

  ##	In this section we make the model.matrix X and the sub
  ##	model.matices X1 and X2 such that X1 contains all of the
  ##	columns of X that correspond to factor variables and X2
  ##	contains all the columns of X that correspond to numeric
  ##	variables.
  ##    x1.idx[] are the indices of columns of X that are in X1.

  X <- model.matrix(Terms, m, contrasts)
  if (nrow(X) <= ncol(X))
    stop("Robust method is inappropriate: not enough observations.")

  asgn <- attr(X, "assign")
  if(!is.list(asgn))
    asgn <- splus.assign(asgn, attr(Terms, "term.labels"))

  factor.names <- names(m)[sapply(m, is.factor)]
  x1.idx <- unname(unlist(asgn[factor.names]))

  X1 <- X[, x1.idx, drop = FALSE]
  X2 <- X[, setdiff(1:(dim(X)[2]), x1.idx), drop = FALSE]

  ##	If there are both factor and numeric variables
  ##	then put the intercept term in X1
  p2 <- ncol(X2)
  cnam2 <- dimnames(X2)[[2]]
  if(dim(X1)[2] > 0 && p2 > 1 && cnam2[1] == "(Intercept)") {
    X1 <- cbind(X2[, 1], X1)
    dimnames(X1)[[2]][1] <- "(Intercept)"
    X2 <- X2[, -1, drop = FALSE]
    x1.idx <- c(1L, x1.idx)
  }
  ##	If the only column in X2 is the intercept
  ##	move it to X1 and set X2 to NULL.
  else if(p2 == 1 && cnam2 == "(Intercept)") {
    X1 <- X
    x1.idx <- 1:ncol(X)
    X2 <- NULL
  }

  ##	If X1 is empty then set it to NULL.
  if(length(dim(X1)) && dim(X1)[2] == 0) {
    X1 <- NULL
    x1.idx <- NULL
  }

  ##	If X2 is empty then set it to NULL.
  if(length(dim(X2)) && dim(X2)[2] == 0)
    X2 <- NULL

  fit <-
      if(length(weights))
	  lmRob.wfit(X, Y, weights, x1=X1, x2=X2, x1.idx=x1.idx, nrep=nrep,
		     robust.control = control, genetic.control=genetic.control)
      else
	  lmRob.fit(X, Y, x1=X1, x2=X2, x1.idx=x1.idx, nrep=nrep,
		    robust.control = control, genetic.control=genetic.control)
  if(is.null(fit))
    return(NULL)
  fit$terms <- Terms
  fit$call <- the.call

  x.names <- dimnames(X)[[2]]
  pasgn <- asgn
  qrx <- qr(X)
  Rk <- qrx[["rank"]]
  piv <- qrx[["pivot"]][1:Rk]
  newpos <- match(1:Rk, piv)
  if(length(x.names))
    names(newpos) <- x.names
  for(j in names(asgn)) {
    aj <- asgn[[j]]
    aj <- aj[ok <- (nj <- newpos[aj]) <= Rk]
    if(length(aj)) {
      asgn[[j]] <- aj
      pasgn[[j]] <- nj[ok]
    }
    else
      asgn[[j]] <- pasgn[[j]] <- NULL
  }

  effects <- X * matrix(fit$coefficients, byrow=TRUE, nrow=nrow(X), ncol=ncol(X))
  fit$effects <- sqrt(colSums(effects^2))
  fit$assign <- asgn
  if (model)
    fit$model <- m
  if (x)
    fit$x <- X
  if (y)
    fit$y <- Y
  attr(fit, "na.message") <- attr(m, "na.message")
  if (!is.null(attr(m, "na.action")))
    fit$na.action <- attr(m, "na.action")
  fit
}


