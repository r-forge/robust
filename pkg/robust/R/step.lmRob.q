step.lmRob <- function(object, scope, scale, direction = c("both", "backward", "forward"),
                       trace = TRUE, keep = NULL, steps = 1000, fast = FALSE, ...)
{
  if(missing(direction))
    direction <- "backward"

  else direction <- match.arg(direction)

  if(direction != "backward")
    stop("Presently step.lmRob only supports backward model selection.")

# sub.assign <- function(terms, assign)
# {
#   a <- attributes(terms)
#   tl <- a$term.labels
#   if(a$intercept)
#     tl <- c(names(assign)[1], tl)
#   asgn <- assign[tl]
#   poi <- 0
#   for(i in tl) {
#     la <- length(asgn[[i]])
#     asgn[[i]] <- seq(poi + 1, poi + la)
#     poi <- poi + la
#   }
#   asgn
# }

  re.arrange <- function(keep)
  {
    namr <- names(k1 <- keep[[1]])
    namc <- names(keep)
    nc <- length(keep)
    nr <- length(k1)
    array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
  }

  make.step <- function(models, fit, scale, object)
  {
    change <- sapply(models, "[[", "change")
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, diff(rdf))
    RFPE <- sapply(models, "[[", "RFPE")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
      "\nInitial Model:", deparse(as.vector(formula(object))),
      "\nFinal Model:", deparse(as.vector(formula(fit))),
      "\n")
    aod <- data.frame(Step = change, Df = ddf, "Resid. Df" = rdf,
      RFPE = RFPE, check.names = FALSE)
    attr(aod, "heading") <- heading
    oldClass(aod) <- c("anova", "data.frame")
    fit$anova <- aod
    fit
  }

  backward <- direction == "both" || direction == "backward"
  forward <- direction == "both" || direction == "forward"

  if(missing(scope)) {
    fdrop <- numeric(0)
    fadd <- NULL
  }

  else {
    if(is.list(scope)) {
      fdrop <- if(!is.null(fdrop <- scope$lower)) attr(terms(
          update.formula(object, fdrop)), 
          "factor") else numeric(0)
      fadd <- if(!is.null(fadd <- scope$upper)) attr(terms(
          update.formula(object, fadd)), "factor")
    }
    else {
      fadd <- if(!is.null(fadd <- scope))
        attr(terms(update.formula(object, scope)), "factor")
      fdrop <- numeric(0)
    }
  }

  if(is.null(fadd)) {
    backward <- TRUE
    forward <- FALSE
  }

  m <- model.frame(object)
  obconts <- object$contrasts
  objectcall <- object$call
  robust.control <- object$robust.control

  #build the big model matrix
  if(forward) {
    add.rhs <- paste(dimnames(fadd)[[2]], collapse = "+")
    add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
    new.form <- update.formula(object, add.rhs, evaluate = FALSE)
    fc <- objectcall
    Terms <- terms(new.form)
    fc$formula <- Terms
    fobject <- list(call = fc)
    oldClass(fobject) <- oldClass(object)
    m <- model.frame(fobject)
    x <- model.matrix(Terms, m, contrasts = obconts)
  }

  else {
    Terms <- object$terms
    x <- model.matrix(Terms, m, contrasts = obconts)
  }



  Asgn <- attr(x, "assign")
  term.labels <- attr(Terms, "term.labels")

#  if(!is.list(Asgn))
#    Asgn <- splus.assign(Asgn, term.labels)

  a <- attributes(m)
  y <- model.extract(m, "response")
  w <- model.extract(m, "weights")

  if(is.null(w))
    w <- rep(1, nrow(m))

  models <- vector("list", steps)

  if(!is.null(keep)) {
    keep.list <- vector("list", steps)
    nv <- 1
  }

  n <- length(object$fitted)
  scale <- object$scale
  fit <- object
# cf <- attributes(coef(object))

  #check if any terms have zero df
# if(cf$singular) {
#   TT <- !match(TL <- attr(object$terms, "term.labels"), names(cf$assign), FALSE)
#   if(any(TT)) {
#     upd <- eval(parse(text = paste(c(".~.", TL[TT]), collapse = "-")))
#     fit <- update(fit, upd)
#   }
# }

  bRFPE <- lmRob.RFPE(fit)
  nm <- 1
  Terms <- fit$terms
  if(trace)
    cat("Start:  RFPE=", format(round(bRFPE, 4)), "\n",
      deparse(as.vector(formula(fit))), "\n\n")

  models[[nm]] <- list(df.resid = fit$df.resid, change = "", RFPE = bRFPE)

  if(!is.null(keep))
    keep.list[[nm]] <- keep(fit, bRFPE)

  RFPE <- bRFPE + 1

  while(bRFPE < RFPE & steps > 0) {
    steps <- steps - 1
    RFPE <- bRFPE
    bfit <- fit
    ffac <- attr(Terms, "factor")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL

    if(backward && (ndrop <- length(scope$drop))) {
      aod <- drop1.lmRob(fit, scope$drop, scale)
      if(trace)
        print(aod)
      change <- rep("-", ndrop + 1)
    }

    if(forward && (nadd <- length(scope$add))) {
      aodf <- add1.lmRob(fit, scope$add, scale, x = x)
      if(trace)
        print(aodf)
      change <- c(change, rep("+", nadd + 1))
      if(is.null(aod))
        aod <- aodf
      else {
        ncaod <- dim(aod)[1]
        aod[seq(ncaod + 1, ncaod + nadd + 1),  ] <- aodf
      }
    }

    if(is.null(aod))
      break

    o <- order(aod[, "RFPE"])[1]

    ## If the original model has minimum RFPE then break
    if(o[1] == 1) break

    change <- paste(change[o], dimnames(aod)[[1]][o])
    Terms <- terms(update(formula(fit), eval(parse(text = paste("~ .", change)))))
    attr(Terms, "formula") <- new.formula <- formula(Terms)
    #asgn <- sub.assign(Terms, Asgn)
    #tx <- x[, unlist(Asgn[names(asgn)]), drop = FALSE]
    newfit <- lmRob(new.formula, data = m, control = robust.control)
    bRFPE <- aod[, "RFPE"][o]

    if(trace)
      cat("\nStep:  RFPE =", format(round(bRFPE, 4)), "\n",
        deparse(as.vector(formula(Terms))), "\n\n")

    if(bRFPE >= RFPE)
      break

    nm <- nm + 1
    models[[nm]] <- list(df.resid = newfit$df.resid, change = change, RFPE = bRFPE)
    fit <- c(newfit, list(formula = new.formula))
    oc <- objectcall
    oc$formula <- as.vector(fit$formula)
    fit$call <- oc
    oldClass(fit) <- oldClass(object)
    if(!is.null(keep))
      keep.list[[nm]] <- keep(fit, bRFPE)
  }

  if(!is.null(keep))
    fit$keep <- re.arrange(keep.list[seq(nm)])

  make.step(models = models[seq(nm)], fit, scale, object)
}


add1.lmRob <- function(u, v, w, x)
  stop("add1.lmRob is not implemented")


