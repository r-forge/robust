anova.lmRoblist <- function(object, const, ipsi, yc, 
    test = c("RWald", "RF"), ...) 
{
  diff.Rn2 <- function(term.coefs, term.cov) {
    t1 <- attr(term.coefs[[1]],"names")
    t2 <- attr(term.coefs[[2]],"names")
    m1 <- match(t1, t2, FALSE)
    m2 <- match(t2, t1, FALSE)
    if(all(m1)) {
      if(all(m2)) 
        return(list(effects="No sub-model",
                    df=NA, Chi.val=NA, Prob.Chi=NA))
      else {
        lab0 <- t2[-m1]
        cov0 <- term.cov[[2]][lab0,lab0]
        coef0 <- term.coefs[[2]][lab0]
        inv0 <- solve(cov0)
        coef0 <- as.matrix(coef0)
        val <- as.numeric( t(coef0) %*% inv0 %*% coef0 )
        df <- length(lab0)
        P <- 1 - pchisq(val, df)

        return(list(effects=paste(c("", t2[ - m1]), collapse = "+"), 
               df=df, Chi.val=val, Prob.Chi=P))
      }
    }

    else {
      if(all(m2)) {
        lab0 <- t1[-m2]
        cov0 <- term.cov[[1]][lab0,lab0]
        coef0 <- term.coefs[[1]][lab0]
        inv0 <- solve(cov0)
        coef0 <- as.matrix(coef0)
        val <- as.numeric( t(coef0) %*% inv0 %*% coef0 )
        df <- length(lab0)
        P <- 1 - pchisq(val, df)

        return(list(effects=paste(c("", t1[ - m2]), collapse = "-"), 
               df=df, Chi.val=val, Prob.Chi=P))
      }

      else return(list(effects="No sub-model",
                  df=NA, Chi.val=NA, Prob.Chi=NA))
    }
  }

  diff.Tau <- function(res, scale, cname, ipsi, yc, const) {     
    t1 <- cname[[1]]
    t2 <- cname[[2]]
    m1 <- match(t1, t2, FALSE)
    m2 <- match(t2, t1, FALSE)
    if(all(m1)) {
      if(all(m2)) 
        return(list(effects="No sub-model",
                    df=NA, Tau.val=NA, Prob.Tau=NA))
      else {
        lab0 <- t2[-m1]
        Scale <- scale[[2]]
        df <- length(lab0)
        FTau <- (2/df)*sum(chi.weight(res[[1]]/Scale,ipsi,yc)-
                            chi.weight(res[[2]]/Scale,ipsi,yc))
        P <- 1 - pchisq(FTau/const, df)

        return(list(effects=paste(c("", t2[ - m1]), collapse = "+"), 
               df=df, Tau.val=FTau, Prob.Tau=P))
      }
    }

    else {
      if(all(m2)) {
        lab0 <- t1[-m2]
        Scale <- scale[[1]]
        df <- length(lab0)
        FTau <- (2/df)*sum(chi.weight(res[[2]]/Scale,ipsi,yc)-
                            chi.weight(res[[1]]/Scale,ipsi,yc))
        P <- 1 - pchisq(FTau/const, df)

        return(list(effects=paste(c("", t1[ - m2]), collapse = "-"), 
               df=df, Tau.val=FTau, Prob.Tau=P))
      }
      else return(list(effects="No sub-model",
                  df=NA, Tau.val=NA, Prob.Tau=NA))
    }
  }

  forms <- sapply(object, function(x) as.character(formula(x)))
  subs <- as.logical(match(forms[2,  ], forms[2, 1], FALSE))

  ns <- sapply(object, function(u) length(residuals(u)))
  if(any(ns != ns[1])) 
    stop("models were not all fitted to the same size dataset")

  if(!all(subs)) 
    warning("Some fit objects deleted because response differs from the first model")

  if(sum(subs) == 1)
    stop("The first model has a different response from the rest")

  forms <- forms[, subs]
  object <- object[subs]
  estype <- sapply(object, "[[", "est")
  subs <- estype == "final"  

  if(!all(subs)) {
    warning(paste("Inference based on initial estimates is followed by",
                 " a (*) sign, and not recommended.", sep=""))
  }

  rt <- length(object)
  if(rt == 1) {
    object <- object[[1]]
    return(anova(object, ...))
    #UseMethod("anova")
  }

  twrn <- rep("   ", rt)
  effects <- character(rt)
  Probchi <- rep(NA,rt)
  Df <- rep(NA,rt)
  Statval <- rep(NA,rt)
  heading <- paste("\nResponse: ", forms[2, 1], sep = "")

  if(test == "RWald") {
    tc <- list()
    tk <- list()
    for (i in 1:rt) {
      tc[[i]] <- object[[i]]$coefficients
      tk[[i]] <- object[[i]]$cov
      if (object[[i]]$est != "final") 
        twrn[i] <- "(*)"
    }

    for(i in 2:rt) {

      j <- c(i-1, i)
      diff.val <- diff.Rn2(tc[j],tk[j]) 
      effects[i] <- diff.val$effects
      Df[i] <- diff.val$df
      Statval[i] <- diff.val$Chi.val
      Probchi[i] <- diff.val$Prob.Chi 
    }
    aod <- data.frame(Terms = forms[3,  ], "   " = twrn, Df = Df,
           "Wald" = Statval, "P(>Wald)" = Probchi, check.names = FALSE)
  }

  else {#Tau - test, Robust F
    rs <- list()
    sc <- list()
    cn <- list()
    for (i in 1:rt) {
      sc[[i]] <- object[[i]]$scale 
      cn[[i]] <- names(object[[i]]$coefficients)
      rs[[i]] <- object[[i]]$residuals
      if (object[[i]]$est != "final") 
        twrn[i] <- "(*)"
    }    

    for(i in 2:rt) {
      j <- c(i-1, i)
      diff.val <- diff.Tau(rs[j],sc[j],cn[j],ipsi,yc,const) 
      effects[i] <- diff.val$effects
      Df[i] <- diff.val$df
      Statval[i] <- diff.val$Tau.val
      Probchi[i] <- diff.val$Prob.Tau
    }
    aod <- data.frame(Terms = forms[3,  ], "   " = twrn, Df = Df,
           "RobustF" = Statval, "Pr(F)" = Probchi, check.names = FALSE)
  }
  oldClass(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- heading

  aod
}

