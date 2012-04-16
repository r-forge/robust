anova.lmRob <- function(object, ..., test = c("RF", "RWald")) 
{
  margs <- function(...) {nargs()}
  
  test <- match.arg(test) 
  psif <- object$robust.control$weight
  efficiency <- object$robust.control$efficiency
  final.alg <- object$robust.control$final.alg

  if(casefold(final.alg) == "adaptive") {
    if(test == "RF")
      stop("Robust F-test is only available for final MM-estimates.")
  }

  else {
    if(casefold(psif[2]) == "optimal") {
      ipsi <- 1
      if(efficiency == 0.95) {
        cst <- 0.976
        yc <- 1.060158 
      }
      else if(efficiency == 0.9) {
        cst <- 0.963
        yc <- 0.9440982
      }
      else if(efficiency == 0.85) {
        cst <- 0.953
        yc <- 0.8684 
      }
      else if(efficiency == 0.8) {
        cst <- 0.944
        yc <- 0.8097795 
      }
      else {
        cst <- lmRob.const(efficiency, ipsi = 1)
        yc <- lmRob.effvy(efficiency, ipsi = 1) 
      }
    }

    else {
      ipsi <- 2
      if(efficiency == 0.95) {
        cst <- 0.218
        yc <- 4.685061 
      }
      else if(efficiency == 0.9) {
        cst <- 0.295
        yc <- 3.882646 
      }
      else if(efficiency == 0.85) {
        cst <- 0.357
        yc <- 3.443689 
      }
      else if(efficiency == 0.8) {
        cst <- 0.413
        yc <- 3.136909 
      }
      else {
        cst <- lmRob.const(efficiency, ipsi = 2)
        yc <- lmRob.effvy(efficiency, ipsi = 2)
      }
    }
  }

  if(margs(...))
    return(anova.lmRoblist(list(object, ...), cst, ipsi, yc, test = test))

  if(object$est == "initial")
    warning("Inference based on initial estimates is not recommended.")

  cov <- object$cov
  coef <- coef(object)
  assg <- object$assign
  term.labels <- attr(object$terms, "term.labels")

  if(is.null(assg))
    assg <- attributes(object$terms)$assign

  if(min(assg) == 0)
    nassg <- c("(Intercept)", term.labels)
  else
    nassg <- term.labels

  aod <- matrix(table(assg), ncol = 1)
  dimnames(aod) <- list(nassg, NULL)

  # looks like there's a bug here when 'Chisq Df' > 1
  if(test == "RWald") {
    rnames <- c("Chisq Df", "Wald", "P(>Wald)")
    aod <- cbind(aod, NA, NA)
    j <- nrow(aod)
    if(j > 1) {
      for(i in 2:j) {
        ch.val <- coef[i] * coef[i] / cov[i, i]
        aod[i, 2] <- ch.val
        aod[i, 3] <- 1 - pchisq(ch.val, 1) 
      } 
    }
  } 

  else {
    rnames <- c("Chisq Df", "RobustF", "Pr(F)")
    aod <- cbind(aod, NA, NA)
    j <- nrow(aod)

    if(nassg[1] == "(Intercept)") 
      frmcar <- ". ~ 1"
    else 
      frmcar <- paste(". ~ -1 +", nassg[1])

    curfrm <- as.formula(frmcar)
    curobj <- update(object, curfrm)

    if(curobj$est == "final") 
      res <- resid(curobj)
    else  
      res <- curobj$T.residuals  

    if(j > 2) {
      for(i in 2:(j-1)) {
        if(frmcar == ". ~ 1") 
          frmcar <- paste(". ~", nassg[i])
        else 
          frmcar <- paste(frmcar, nassg[i], sep=" + ")

        curfrm <- as.formula(frmcar)
        curobj <- update(object,curfrm)

        if(curobj$est == "final") 
          Res <- resid(curobj)
        else  
          Res <- curobj$T.residuals  

        Scale <- curobj$scale 
        FTau <- 2 * sum(chi.weight(res / Scale, ipsi, yc) -
                        chi.weight(Res / Scale, ipsi, yc)) 
        aod[i, 2] <- FTau
        aod[i, 3] <- 1 - pchisq(FTau / cst, 1) 
        res <- Res
      }
    }

    if(object$est == "final") 
      Res <- resid(object)
    else  
      Res <- object$T.residuals

    Scale <- object$scale 
    FTau <- 2*sum(chi.weight(res / Scale, ipsi, yc)-
                  chi.weight(Res / Scale, ipsi, yc)) 

    aod[j, 2] <- FTau  
    aod[j, 3] <- 1 - pchisq(FTau / cst, 1) 
  } 

  dimnames(aod) <- list(nassg, rnames)
  heading <- "\nTerms added sequentially (first to last)\n"

  aod <- data.frame(aod, check.names = FALSE)
  oldClass(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- heading

  aod
}


