anova.glmRob <- function(object, ..., test = c("none", "Chisq", "F", "Cp"))
{
  test <- match.arg(test)

  margs <- function(...)
    nargs()

  if(margs(...))
    return(anova.glmRoblist(list(object, ...), test = test))

  Terms <- object$terms
  term.labels <- attr(Terms, "term.labels")
  nt <- length(term.labels)
  m <- model.frame(object)
  x <- model.matrix(Terms, m, contrasts = object$contrasts)
  asgn <- attr(x, "assign")

  control <- object$control

  if(is.null(control)) {
    fit.method <- object$fit.method
    if(fit.method=="cubif") 
      control <- glmRob.cubif.control()
    else if(fit.method == "mallows")
      control <- glmRob.mallows.control()
    else if(fit.method == "misclass")
      control <- glmRob.misclass.control()
    else 
      stop(paste("method ", fit.method," does not exist"))
  }

  Family <- family(object)
  a <- attributes(m)
  y <- model.extract(m, "response")
  w <- model.extract(m, "weights")

  if(!length(w))
    w <- rep(1, nrow(m))

  offset <- attr(Terms, "offset")

  if(is.null(offset))
    offset <- 0

  else
    offset <- m[[offset]]

  dev.res <- double(nt)
  df.res <- dev.res
  nulld <- object$null.deviance

  if(is.null(nulld))
    nulld <- sum(w * (y - weighted.mean(y, w))^2)

  dev.res[1] <- nulld
  df.res[1] <- nrow(x) - attr(Terms, "intercept")

  if(nt > 1) {
    for(iterm in seq(nt, 2)) {

      idx <- which(asgn == iterm)
      x <- x[ , -idx, drop = FALSE]
      asgn <- asgn[-idx]

      fit.call <- object$call
      fit.call[[1]] <- as.name(paste('glmRob.', object$method, sep = ''))
      fit.call$x <- x
      fit.call$y <- y
      fit.call$control <- control
      fit.call$family <- family(object)
      fit.call$offset <- offset
      fit.call$Terms <- NULL
      fit.call$null.dev <- TRUE
      fit.call$formula <- NULL
      fit.call$data <- NULL
      fit <- eval(fit.call, sys.parent())

      dev.res[iterm] <- deviance(fit)
      df.res[iterm] <- fit$df.resid
    }
  }

  if(nt) {
    dev.res <- c(dev.res, deviance(object))
    df.res <- c(df.res, object$df.resid)
    dev <- c(NA,  - diff(dev.res))
    df <- c(NA,  - diff(df.res))
  }

  else dev <- df <- as.numeric(NA)  

  heading <- c("Analysis of Deviance Table\n", 
      paste(Family$family[1], "model\n"), 
      paste("Response: ", 
        as.character(formula(object))[2], 
        "\n", sep = ""), 
      "Terms added sequentially (first to last)")

  aod <- data.frame(Df = df, Deviance = dev, 
      "Resid. Df" = df.res, "Resid. Dev" = dev.res, 
      row.names = c("NULL", term.labels), 
      check.names = FALSE)

  attr(aod, "heading") <- heading

  oldClass(aod) <- c("anova", "data.frame")

  if(test == "none")
    return(aod)

  else
    stat.anova(aod, test, 
      deviance(object)/object$df.resid, 
      object$df.resid, nrow(x))
}



