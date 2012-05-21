#----------------------------------------------------------------#
# Robust Linear Regression                                       #
# Author: Jeffrey Wang & Kjell Konis                             #
# Date:   02/06/2002                                             #
# Insightful Corporation                                         #
#----------------------------------------------------------------#

lmRob <- function(formula, data, weights, subset, na.action,
                  model = TRUE, x = FALSE, y = FALSE, contrasts = NULL,
                  nrep = NULL, control = lmRob.control(...), ...)
{
  the.call <- match.call()

  m.call <- match.call(expand.dots = FALSE)
  m.args <- match(c("formula", "data", "subset", "weights", "na.action"),
                  names(m.call), 0)
  m.call <- m.call[c(1, m.args)]
  m.call$drop.unused.levels <- TRUE
  m.call[[1]] <- as.name("model.frame")
  m <- eval(m.call, parent.frame())

  Terms <- attr(m, "terms")
  weights <- model.extract(m, "weights")
  Y <- model.extract(m, "response")
  X <- model.matrix(Terms, m, contrasts)

  if(nrow(X) <= ncol(X))
    stop("Robust method is inappropriate: there are not enough observations")

  ## Find the columns of the model matrix X arising from coding
  ## factor variables and store their indices in x1.idx.
  ## Conceptually, the design matrix  X = [ x1 | x2 ].

  asgn <- attr(X, "assign")
  factor.vars <- names(m)[sapply(m, is.factor)]
  factors <- attr(Terms, "factors")

  if(length(factors))
    factors <- factors[factor.vars, , drop = FALSE]
  else
    factors <- matrix(0.0, nrow = 0, ncol = 0)

  order <- attr(Terms, "order")
  intercept <- attr(Terms, "intercept")

  if(nrow(factors) > 0)
    all.factors <- apply(factors > 0, 2, sum) == order
  else
    all.factors <- logical(0)

  x1.idx <- which(asgn %in% which(all.factors))
  x2.idx <- setdiff(1:(dim(X)[2]), x1.idx)

  if(intercept == 1) {
    ##  If there are both factor and numeric variables
    ##  then put the intercept term in X1

    if(length(x1.idx) > 0 && length(x2.idx) > 1) {
      x1.idx <- c(1, x1.idx)
      x2.idx <- x2.idx[-1]
    }

    ##  If the only column in X2 is the intercept
    ##  move it to X1 and set X2 to NULL.

    else if(length(x2.idx) == 1) {
      x1.idx <- c(1, x1.idx)
      x2.idx <- x2.idx[-1]
    }
  }

  if(length(weights))
    fit <- lmRob.wfit(X, Y, weights, x1.idx = x1.idx, nrep = nrep,
                      robust.control = control)
  else
    fit <- lmRob.fit(X, Y, x1.idx = x1.idx, nrep = nrep,
                     robust.control = control)

  if(is.null(fit))
    return(NULL)

  fit$terms <- Terms
  fit$call <- the.call

  effects <- X * matrix(fit$coefficients, nrow(X), ncol(X), byrow = TRUE)
  fit$effects <- sqrt(colSums(effects^2))
  fit$assign <- asgn
  if(model)
    fit$model <- m
  if(x)
    fit$x <- X
  if(y)
    fit$y <- Y
  attr(fit, "na.message") <- attr(m, "na.message")
  if(!is.null(attr(m, "na.action")))
    fit$na.action <- attr(m, "na.action")

  fit
}


