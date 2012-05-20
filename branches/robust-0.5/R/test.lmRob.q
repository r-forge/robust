test.lmRob <- function(object, type = "bias", level = NULL, n.permute = 99)
{

##
## Step 1. Construct Response Vector and Model Frame
##

  contrasts <- object$contrasts
  m <- object$m

  Terms <- attr(m, "terms")
  weights <- model.extract(m, "weights")
  y <- model.extract(m, "response")
  x <- model.matrix(Terms, m, contrasts)
  n <- length(y)
  p <- length(coef(object))
  control <- object$robust.control

  if(casefold(type) == "permutation") {
    x.nam <- dimnames(x)[[2]]

    if(x.nam[1] == "(Intercept)")
      x <- x[,-1]

    if(!is.null(dim(x)))
      stop("Permutation test is only available for a straight line fit.")

    control$initial.alg <- "Random"
    gen.list <- object$genetic.control
    #perm.x <- samp.permute(1:n, n.permute)
    perm.x <- sapply(rep(n, n.permute), function(u) sample(1:u))
    beta.all <- rep(0, n.permute)

    for(i in 1:n.permute) {
      tmp <- lmRob(y~x[perm.x[,i]],robust.control=control,
                   genetic.control=gen.list)
      beta.all[i] <- coef(tmp)[2]
    }

    beta.all <- abs(c(0, beta.all))
    k <- (1:(n.permute+1))[order(beta.all) == 1] - 1
    return((n.permute+1-k)/(n.permute+1))
  }

  if(casefold(type) != "bias")
    stop("Wrong type for test.lmRob")

  tmp <- control$final.alg
  if (casefold(tmp) == "adaptive")
    stop("Tests for bias are only available for MM-estimates.")

##
## Step 2. Extract Components from Model Object
##

  rank <- object$rank
  resid0 <- object$T.residuals
  scale0 <- object$scale
  scale1 <- object$T.scale
  tl <- control$tl
  psi <- control$weight

  if(is.null(level))
    level <- 1-control$level

  eff <- control$efficiency

  if(casefold(psi[1]) == "bisquare") {
    ipsi <- 2
    xk <- 1.5477
  }

  else if(casefold(psi[1]) == "optimal") {
    ipsi <- 1
    xk <- 0.4047
  }

  else 
    stop("Invalid choice of weight function.")

  if(casefold(psi[2]) == "optimal") {
    ipsi2 <- 1
    if (eff == 0.95)      yc <- 1.060158
    else if (eff == 0.9)  yc <- 0.9440982
    else if (eff == 0.85) yc <- 0.8684
    else if (eff == 0.8)  yc <- 0.8097795
    else                  yc <- lmRob.effvy(eff)
  }

  else if(casefold(psi[2]) == "bisquare") {
    ipsi2 <- 2
    if (eff == 0.95) yc <- 4.685061
    else if (eff == 0.9)  yc <- 3.882646
    else if (eff == 0.85) yc <- 3.443689
    else if (eff == 0.8)  yc <- 3.136909
    #else                  yc <- chb(eff)$cb
    else                  yc <- lmRob.effvy(eff, ipsi = 2)
  }

  else 
    stop("Invalid choice of weight function.")

##
## Step 3. Compute Test Statistics
##

  #b.OLS <- solve(x, y)
  b.OLS <- lsfit(x, y, intercept = FALSE)$coef
  resid.OLS <- y - x %*% b.OLS
  scale.OLS <- sqrt(sum(resid.OLS * resid.OLS)/(n-p))
  tmp <- resid0/scale0
  sc0.OLS <- tmp
  sc0 <- psi.weight(tmp, ipsi2, yc)
  s1p <- sum(psp.weight(tmp, ipsi2, yc))/n
  s0p <- sum(psp.weight(tmp, ipsi, xk))/n
  sc1 <- psi.weight(tmp, ipsi, xk)
  tmp <- tmp * psi.weight(tmp, ipsi, xk)
  s0r <- (sum(tmp) * scale0)/n

  if(s0r < tl || s0p < tl || s1p < tl) 
    warning(paste("Denominator smaller than tl=",tl," in test for bias."))

  tmp <- sc0/s1p - sc1/s0p
  d2 <- sum(tmp * tmp)/n
  tmp <- sc0.OLS - sc1/s0p
  d2.OLS <- sum(tmp * tmp)/n

  if(d2 < tl) 
    warning(paste("Denominator smaller than tl=",tl," in test for bias."))

  tbias <- (2*n*(scale1-scale0)*s0r)/(s0p*d2*scale0*scale0)
  tbias.OLS <- (2*n*(scale.OLS-scale0)*s0r)/(s0p*d2.OLS*scale0*scale0)
  qchi <- qchisq(level, rank)
  pchi <- 1-pchisq(tbias, rank)
  pchi.OLS <- 1-pchisq(tbias.OLS,rank)

  ans <- matrix(c(tbias, tbias.OLS, pchi, pchi.OLS), 2, 2)
  dimnames(ans) <- list(c("M-estimate", "LS-estimate"), c("statistic", "p-value"))
  ans

#  mm.bias <- list(stat=tbias, pchi=pchi, qchi=qchi)
#  ls.bias <- list(stat=tbias.OLS, pchi=pchi.OLS)

#  ans <- list(mm = mm.bias, ls = ls.bias, level = level)
#  oldClass(ans) <- "biasMM" 
#  ans
}


