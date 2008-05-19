anova.lmRob <- function(object, ..., test=c("RF","RWald")) 
{
  margs <- function(...) {nargs()}
  fun <- function(asgn, coeff) {sum(!is.na(coeff[asgn]))}
  
  test  <- match.arg(test) 
  psif <- object$robust.control$weight
  efficiency <- object$robust.control$efficiency
  tmp <- object$robust.control$final.alg

  if(casefold(tmp) == "adaptive") {
    if (test == "RF")
      stop("Robust F-test is only available for final MM-estimates.")
  }

  else {
    if (casefold(psif[2]) == "optimal") {
      ipsi <- 1
      if (efficiency == 0.95) {
        cst <- 0.976; yc <- 1.060158 
      }
      else if (efficiency == 0.9) {
        cst <- 0.963; yc <- 0.9440982
      }
      else if (efficiency == 0.85) {
        cst <- 0.953; yc <- 0.8684 
      }
      else if (efficiency == 0.8) {
        cst <- 0.944; yc <- 0.8097795 
      }
      else {
        cst <- lmRob.const(efficiency, ipsi)
        yc <- lmRob.effvy(efficiency) 
      }
    }

    else {
      ipsi <- 2
      if (efficiency == 0.95) {
        cst <- 0.218; yc <- 4.685061 
      }
      else if (efficiency == 0.9) {
        cst <- 0.295; yc <- 3.882646 
      }
      else if (efficiency == 0.85) {
        cst <- 0.357; yc <- 3.443689 
      }
      else if (efficiency == 0.8) {
        cst <- 0.413; yc <- 3.136909 
      }
      else {
        cst <- lmRob.const(efficiency, ipsi)
        yc <- chb(efficiency)$cb 
      }
    }
  }

  if(margs(...))
    return(anova.lmRoblist(list(object, ...), cst, ipsi, yc, test = test))

  if (object$est == "initial")
    warning("Inference based on initial estimates is not recommended.")

  cov <- object$cov
  coef <- object$coef
  np <- length(coef)
  assg <- object$assign
  term.labels <- attr(object$terms, "term.labels")

  if(is.null(assg))
    assg <- attributes(object$terms)$assign

  if(!is.list(assg))
    assg <- splus.assign(assg, term.labels)

  df <- sapply(assg, fun, object$coef)
  nassg <- names(assg)
  names(df) <- nassg

  if (test == "RWald") {
    aod <- as.matrix(round(df, 1))
    rnames <- c("Chisq Df", "Wald", "P(>Wald)")
    aod <- cbind(aod, NA, NA)
    j <- length(df)
    if (j>1) {
      for (i in 2:j) {
        ch.val   <- coef[i]*coef[i]/cov[i,i]
        aod[i,2] <- ch.val
        aod[i,3] <- 1 - pchisq(ch.val,1) 
      } 
    }
  } 

  else {
    aod <- as.matrix(round(df, 1))
    rnames <- c("Chisq Df", "RobustF","Pr(F)")
    aod <- cbind(aod, NA, NA)
    j <- length(df)

    if (nassg[1] == "(Intercept)") 
      frmcar <- ".~1"

    else 
      frmcar <- paste(".~ -1 +",nassg[1])

    curfrm <- as.formula(frmcar)
    curobj <- update(object, curfrm)

    if (curobj$est == "final") 
      res <- curobj$residuals

    else  
      res <- curobj$T.residuals  

    if (j>2) {
      for (i in 2:(j-1)) {
        if (frmcar==".~1") 
          frmcar <- paste(".~",nassg[i])
        else 
          frmcar <- paste(frmcar,nassg[i],sep="+")
        curfrm <- as.formula(frmcar)
        curobj <- update(object,curfrm)
        if (curobj$est == "final") 
          Res <- curobj$residuals
        else  
          Res <- curobj$T.residuals  
        Scale <- curobj$scale 
        FTau <- 2*sum(chi.weight(res/Scale,ipsi,yc)-
                      chi.weight(Res/Scale,ipsi,yc)) 
        aod[i,2] <- FTau
        aod[i,3] <- 1 - pchisq(FTau/cst,1) 
        res <- Res
      }
    }

    if (object$est == "final") 
      Res <- object$residuals

    else  
      Res <- object$T.residuals
    Scale <- object$scale 
    FTau <- 2*sum(chi.weight(res/Scale,ipsi,yc)-
                  chi.weight(Res/Scale,ipsi,yc)) 
    aod[j,2] <- FTau  
    aod[j,3] <- 1 - pchisq(FTau/cst,1) 
  } 

  dimnames(aod) <- list(names(df), rnames)
  heading <- "\nTerms added sequentially (first to last)\n"

  aod <- data.frame(aod, check.names = FALSE)
  oldClass(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- heading

  aod
}


