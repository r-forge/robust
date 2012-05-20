drop1.lmRob <- function(object, scope, scale, keep, fast = FALSE, ...)
{
  tmp <- object$robust.control$final.alg

  if(casefold(tmp) == "adaptive")
    stop("drop1 is only available for MM-estimates.")

  x <- model.matrix(object)
  asgn <- attr(x, "assign")
  term.labels <- attr(object$terms, "term.labels")

  dfs <- table(asgn[asgn > 0])
  names(dfs) <- term.labels

  psif <- object$robust.control$weight

  if(missing(scope))
    scope <- drop.scope(object)

  else {
    if(!is.character(scope))
      scope <- attr(terms(update.formula(object, scope)), "term.labels")

    if(!all(match(scope, term.labels, FALSE)))
      stop("scope is not a subset of term labels")
  }

  dfs <- dfs[scope]
  k <- length(scope)

  if(missing(scale))
    scale <- object$scale

  if(object$est == "initial")
    warning("Inference based on initial estimates is not recommended.")

  if(!missing(keep)) {
    max.keep <- c("coefficients", "fitted", "residuals")

    if(is.logical(keep) && keep) 
      keep <- max.keep

    else {
      if(!all(match(keep, max.keep, FALSE)))
        stop(paste("Can only keep one or more of: \"",
                   paste(max.keep, collapse = "\", \""), "\"", sep = ""))
    }

    value <- array(vector("list", 3 * k), c(k, 3),
                   list(scope, c("coefficients", "fitted", "residuals")))
  }

  else
    keep <- character(0)

  rfpe <- double(k)

  if(fast) {
    warning("The fast algorithm in drop1.lmRob is not very reliable.")
    stop("The fast algorithm in drop1.lmRob is broken in this version of the Robust Library")

    Weights <- object$M.weights
    ipsi <- 1
    xk <- .9440982
    itype <- 1
    isigma <- -1
    yc <- object$yc
    mxr <- object$robust.control$mxr
    mxs <- object$robust.control$mxs
    tlo <- object$robust.control$tlo
    tua <- object$robust.control$tua
    tl <- object$robust.control$tl
    y <- object$fitted.values + object$residuals
    n <- length(y)
    tmpn <- double(n)
    beta <- sum(Weights)/(2*n)
    bet0 <- 1

    rfpe.compute <- function(res, scale, ipsi, yc, p) {
      res <- res / scale
      a <- sum(rho.weight(res, ipsi, yc))
      b <- (p * sum(psi.weight(res, ipsi, yc)^2))
      d <- sum(psp.weight(res, ipsi, yc))

      if(d <= 0)
        return(NA)

      a + b/d
    }

    for(i in 1:k) {
      ii <- asgn[[i]]
      pii <- length(ii)
      dfs[i] <- pii

      curx <- x[, -ii, drop = FALSE]
      curobj <- lsfit(x = curx, y = y, wt = Weights, intercept = FALSE)
      coeff0 <- curobj$coef

      scale0 <- .Fortran("rlrsigm2",
                          as.double(curobj$residuals),
                          as.double(curobj$residuals),
                          as.double(mad(curobj$residuals)),
                          as.integer(n),
                          as.integer(dim(curx)[2]),
                          as.double(tlo),
                          as.integer(1),
                          as.integer(1),
                          as.integer(mxs),
                          as.integer(1),
                          SIGMAF = double(1),
                          as.double(tmpn),
                          as.double(tmpn),
                          as.integer(ipsi),
                          as.double(xk),
                          as.double(beta),
                          as.double(bet0),
                          PACKAGE = "robust")$SIGMAF

      ucov0 <- length(y) * solve(t(sqrt(Weights) * curx) %*%
        (sqrt(Weights) * curx)) %*% t(Weights * curx) %*%
        (Weights * curx) %*% solve(t(sqrt(Weights) * curx) %*%
        (sqrt(Weights) * curx))

      ucov0 <- ucov0[row(ucov0) <= col(ucov0)]

      curobj <- lmRob.wm(x = curx, y = y, coeff0 = coeff0,
                ucov0 = ucov0, scale0 = scale0, itype = itype, isigma = isigma,
                ipsi = ipsi, xk = xk, beta = beta, wgt = y, tlo = tlo, tua = tua,
                mxr = mxr)

      rfpe[i] <- rfpe.compute(curobj$rs, scale, ipsi, yc, ncol(curx) - 1)

      if(length(keep)) {
        value[i,1] <- list(curobj$theta)
        value[i,2] <- list(curx %*% matrix(curobj$theta, ncol = 1))
        value[i,3] <- list(curobj$rs)
      }
    }
  }

  else {
    for(i in 1:k) {
      curfrm <- as.formula(paste(".~.-", scope[[i]]))
      curobj <- update(object, curfrm)
      rfpe[i] <- lmRob.RFPE(curobj, scale)
      if(length(keep)) {
        value[i,1] <- list(curobj$coefficients)
        value[i,2] <- list(curobj$fitted)
        value[i,3] <- list(curobj$residuals)
      }
    }
  }
  
  scope <- c("<none>", scope)
  dfs <- c(0, dfs)
  rfpe <- c(lmRob.RFPE(object, scale), rfpe)
  dfs[1] <- NA
  aod <- data.frame(Df=dfs, RFPE=rfpe, row.names=scope, check.names=FALSE)

  head <- c("\nSingle term deletions", "\nModel:",
            deparse(as.vector(formula(object))))

  if(!missing(scale))
    head <- c(head, paste("\nscale: ", format(scale), "\n"))

  oldClass(aod) <- c("anova", "data.frame")
  
  attr(aod, "heading") <- head

  if(length(keep)) {
    value <- value[, keep, drop = FALSE]
    oldClass(value) <- "matrix"
    list(anova = aod, keep = value)
  }
  
  else 
    aod
}




