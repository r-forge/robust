#### Testing  lmRob()   -*- R -*-
####
## Original
## author: Jeffrey Wang
## date  : 08/09/2000
##

{
## Generate some data for loop test ##
  mode(gen.data <- function(coeff, n = 100, eps = 0.1, sig = 3,
                            snr = 1/20, seed = 837)
       {
         set.seed(seed)
         x <- cbind(rnorm(n, 1), rnorm(n, 1)^3, exp(rnorm(n, 1)))
         ru <- runif(n)
         n1 <- sum(ru < eps)
         u <- numeric(n)
         u[ru < eps] <- rnorm(n1, sd = sig/snr)
         u[ru > eps] <- rnorm(n - n1, sd = sig)
         data.frame(y = x %*% matrix(coeff, ncol = 1) + u,
                    x1 = x[, 1], x2 = x[, 2], x3 = x[, 3], x4 = rnorm(n, 1))
       }
  ) == "function"
}

{
  class(simu.dat <- gen.data(1:3)) == "data.frame"
}

{
## test S-estimates with random resampling ##
  m <- lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
             control = lmRob.control(estim = "initial",
             initial.alg = "random"))
  all.equal(unname(coef(m)),
            c(1.659806131, 2.06709376, 2.879434355, -0.2756236906))
}

## {
## ## test S-estimates with genetic algorithm ##
##   all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
##             control = lmRob.control(estim = "initial",
##             initial.alg = "genetic",seed = 100)))),
##             c(0.9202865, 2.046525, 3.063134, -0.2163211),
##             tolerance = 1.0e-6)
## }

{
## test MM-estimates with weight (B,B) ##
    mBB <- lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
                 control = lmRob.control(weight = c("Bisquare", "Bisquare"),
                 efficiency = 0.7, initial.alg = "random", final.alg = "m"))
    all.equal(unname(coef(mBB)),
	      c(1.121617602, 2.028109705, 2.920919887, -0.03255785645))
}

{
## test MM-estimates with weight (B,O) ##
    mBO <- lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
                 control = lmRob.control(weight = c("Bisquare", "Optimal"),
                 efficiency = 0.95, initial.alg = "random", final.alg = "m"))
  all.equal(unname(coef(mBO)),
	    c(1.021358214, 2.040216606, 2.915863868, 0.05542195195))
}

{
## test MM-estimates with weight (O,B) ##
    mOB <- lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
		 control = lmRob.control(weight = c("Optimal", "Bisquare"),
		 efficiency = 0.9,initial.alg = "random", final.alg = "m"))
    all.equal(unname(coef(mOB)),
	      c(1.062536432, 2.035703683, 2.918117149, 0.01624240296))
}

{
## test MM-estimates with weight (O,O) ##
  mOO <- lmRob(y~x1+x2+x3+x4-1, data = simu.dat,
	       control = lmRob.control(weight = c("Optimal","Optimal"),
	       efficiency = 0.85, initial.alg = "random",final.alg = "m"))
  all.equal(as.vector(coef(mOO)),
	    c(1.020023715, 2.040035389, 2.91604064, 0.05466841575))
}

{
## test Robust Wald test ##
  all.equal(anova(mOO, test = "RWald")[,"P(>Wald)"][2:4],
	    c(0, 0, 0.842332812))
}

{
## test Robust F test ##
  all.equal(anova(mOO,test = "RF")[,"Pr(F)"][2:4],
	    c(0, 0, 0.845138356))
}

## {
## ## test REWLS with oilcity data ##
##   tmp <- lmRob(Oil~Market, data = oilcity, control =
##                lmRob.control(efficiency = 0.77,
##                initial.alg = "random",final = "adaptive"))
##   all.equal(as.vector(tmp$coef),
##             c(-0.07813668, 0.8574827),
##             tolerance = 1.0e-6)
## }
{
## test REWLS with coleman data ##
  data(coleman, package = "robustbase")
  mCM <- lmRob(Y ~ . , data = coleman,
               control = lmRob.control(efficiency = 0.77,
               initial.alg = "random", final = "adaptive"))

  all.equal(unname(coef(mCM)),
            c(29.7577177, -1.69854147, 0.0851182371,
              0.666168644, 1.18399532, -4.06675281), tol = 1e-6)
}



{
## test REWLS with stack.loss data ##
  data(stack.dat)
  tmp <- lmRob(Loss~.-1, data = stack.dat, control =
               lmRob.control(weight = "Bisquare", initial.alg = "random",
                             efficiency = 0.77, final.alg = "adaptive"))
  all.equal(as.vector(tmp$coef),
            c(0.6127073, 0.9676439, -0.473352),
            tolerance = 1.0e-6)
}

{
## test robust "mixed" linear models with wagner data
## In the future,  use
##  data(wagnerGrowth, package = "robustbase")
    source(system.file("datasets", "wagner.q",
                       package = "robust")) # wagnerGrowth
    ## 21 levels + 3 levels + 4 continuous :
    tmp <- lmRob(y ~ Region + Period + ., data = wagnerGrowth)
    all.equal(unname(coef(tmp)),
              c(-58.48739738,
                4.24094749, 28.95751724, 25.57747551, 22.72475947, -0.9850417527,
                10.7973689, 23.58086125, 14.47839294, 14.22681835, 8.319455272,
                10.35773846, 15.3466895, 10.36446368, 2.029283378, -8.077244089,
                6.805266348, 12.66957858, 5.855703339, 3.350434134, -6.422418986,
                8.761413075, 16.27819707,
                1.130854624, 0.3911569697, 3.726122795, 2.790172641),
	      tol = 1e-5)

    ## now with non-default control :
    tmp2 <- lmRob(y ~ Region + Period + ., data = wagnerGrowth,
                 control = lmRob.control(weight = "Bisquare",
                 efficiency = 0.77, final.alg = "adaptive"))
    ## FIXME ?: This seems completely platform(?) dependent
}

{
## test fast procedure for lmRob ##
  data(stack.dat)
  tmp <- lmRob(Loss~., data = stack.dat, control = lmRob.control(
               estim = "initial", initial.alg = "Fast"))
  all.equal(c(as.vector(tmp$coef), tmp$scale), c(-35.64108, 0.8458725,
              0.4452125, -0.08965558, 1.837017), tolerance = 1e-05)
}

{
## remove function ###
  rm(gen.data, simu.dat, .Random.seed, tmp, tmp2, m, mBB)
  TRUE
}

