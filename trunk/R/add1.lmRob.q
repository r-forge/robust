add1.lmRob <- function(object, scope = . ~ ., scale, keep, ...)
{
  tmp <- object$robust.control$final.alg

  if(casefold(tmp) == "adaptive")
    stop("add1 is only available for final MM-estimates.")

  p <- length(object$coef)

  if(!is.character(scope))
    scope <- add.scope(object, 
             update.formula(object, scope, evaluate = FALSE))

  lscope <- length(scope)

  if(!lscope)
    stop("no terms in scope for adding to object")

  if(!missing(keep)) {
    max.keep <- c("coefficients", "fitted", "residuals")

    if(is.logical(keep) && keep) 
      keep <- max.keep

    else {
      if(!all(match(keep, max.keep, FALSE)))
        stop(paste("Can only keep one or more of: \"",
             paste(max.keep, collapse="\", \""), "\"", sep=""))
    }
  }

  else
    keep <- character(0)

  value <- array(vector("list", 3 * lscope), c(lscope, 3),
    list(scope, c("coefficients", "fitted", "residuals")))

  dfs <- double(lscope + 1)
  names(dfs) <- c("<none>", scope)
  rfpe <- double(lscope + 1)
  names(rfpe) <- c("<none>", scope)

  if(missing(scale)) 
    scale <- object$scale

	rfpe["<none>"] <- lmRob.RFPE(object, scale)

  for(i in 1:lscope) {
    add.rhs <- eval(parse(text = paste("~ . +", scope[i])))
    new.formula <- update.formula(object, add.rhs, evaluate = FALSE)
    temp <- update(object, formula = new.formula)
    rfpe[scope[i]] <- lmRob.RFPE(temp, scale)
    dfs[scope[i]] <- object$df.residual - temp$df.residual

    if(length(keep))
      value[i, ] <- list(coef(temp), fitted(temp), residuals(temp))
  }

  head <- c("Single term additions", "\nModel:",
    deparse(as.vector(formula(object))),
    if(scale > 0.0) paste("\nscale:", format(scale, ...), "\n"))

  aod <- data.frame(Df = dfs, RFPE = rfpe, row.names = names(dfs),
    check.names = FALSE)

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


