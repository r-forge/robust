##
## test script for lmrob.q
## author: Jeffrey Wang
## date  : 08/09/2000
##

{
### Generate some data for loop test ###
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
### test S-estimates with random resampling ###
  all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data=simu.dat, 
            robust.control=lmRob.robust.control(estim="initial",
            initial.alg="random")))),
            c(0.9202828, 2.046526, 3.063135, -0.2163233), 
            tolerance = 1.0e-6) 
}

{
### test S-estimates with genetic algorithm ###
  all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data=simu.dat, 
            robust.control=lmRob.robust.control(estim="initial",
            initial.alg="genetic",seed=100)))), 
            c(0.9202865, 2.046525, 3.063134, -0.2163211), 
            tolerance=1.0e-6)
}

{
### test MM-estimates with weight (B,B) ###
  all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data=simu.dat, 
            robust.control=lmRob.robust.control(weight=c("Bisquare",
            "Bisquare"), efficiency=0.7, initial.alg="random",
            final.alg="m")))), 
            c(0.6139281, 2.045768, 3.063188, -0.0158846), 
            tolerance=1.0e-6)
}

{
### test MM-estimates with weight (B,O) ###
  all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data=simu.dat, 
            robust.control=lmRob.robust.control(weight=c("Bisquare",
            "Optimal"),efficiency=0.95,initial.alg="random",
            final.alg="m")))), 
            c(0.4079014, 2.05234, 3.025949, 0.07519361), 
            tolerance=1.0e-6)
}

{
### test MM-estimates with weight (O,B) ###
  all.equal(as.vector(coef(lmRob(y~x1+x2+x3+x4-1, data=simu.dat, 
            robust.control=lmRob.robust.control(weight=c("Optimal",
            "Bisquare"),efficiency=0.9,initial.alg="random",
            final.alg="m")))), 
            c(0.5061242, 2.048678, 3.04673, 0.02438666), 
            tolerance=1.0e-6)
}

{
### test MM-estimates with weight (O,O) ###
  tmp <- lmRob(y~x1+x2+x3+x4-1, data=simu.dat,
                  robust.control=lmRob.robust.control(weight=
                  c("Optimal","Optimal"),efficiency=0.85,
                  initial.alg="random",final.alg="m"))
  all.equal(as.vector(coef(tmp)),
            c(0.6335503, 2.048027, 3.045304, -0.05288568), 
            tolerance=1.0e-6)
}

{
### test Robust Wald test ###
  all.equal(anova(tmp, test = "RWald")[[3]][2:4], 
            c(0.0000000, 0.0000000, 0.8331466), 
            tolerance=1.0e-6)
}

{
### test Robust F test ###
  all.equal(anova(tmp,test="RF")[[3]][2:4], 
            c(0.0000000, 0.0000000, 0.8507215), 
            tolerance=1.0e-6) 
}

{
### test REWLS with oilcity data ###
  tmp <- lmRob(Oil~Market, data=oilcity, robust.control=
               lmRob.robust.control(efficiency=0.77, 
               initial.alg="random",final="adaptive"))
  all.equal(as.vector(tmp$coef), 
            c(-0.07813668, 0.8574827),
            tolerance = 1.0e-6) 
}

{
### test REWLS with stack.loss data ###
  tmp <- lmRob(Loss~.-1, data=stack.dat, robust.control=
               lmRob.robust.control(weight="Bisquare",
               initial.alg="random", efficiency=0.77,
               final.alg="adaptive"))
  all.equal(as.vector(tmp$coef), 
            c(0.6127073, 0.9676439, -0.473352),
            tolerance=1.0e-6)
}

{
### test robust mixed linear models with wagner data ###
  tmp <- lmRob(y~Region+Period+PA+GPA+HS+GHS,
               data=wagner.dat, robust.control=
               lmRob.robust.control(weight="Bisquare", 
               efficiency=0.77,final.alg="adaptive"))
  all.equal(as.vector(tmp$coef),
						c(-56.8696680956908, 4.68247883044957, 10.7969838121773,
							4.42592531426025, 2.25823056475092, -2.67283748201289,
							-0.462035180951277, 1.91257088412041, -0.150413716040275,
							0.00920046269046936, -0.710930920231004, -0.339411492220567,
							0.341343150572915, -0.55454618259566, -0.785636250736498,
							-1.42178361709903, -0.448772149504031, 0.0167378476052244,
							-0.509831406604515, -0.581547923891117, -0.711795915838185,
							5.37470544976684, 4.3343399661946, 1.607526983567,
							0.440827499532341, 3.98399527218548, 2.75848348446413),
						tolerance=1.0e-5)
}

{
### test fast procedure for lmRob ###
  tmp <- lmRob(Loss~., data=stack.dat, robust.control=lmRob.robust.control(
               estim="initial", initial.alg="Fast"))
  all.equal(c(as.vector(tmp$coef), tmp$scale), c(-35.64108, 0.8458725,
              0.4452125, -0.08965558, 1.837017), tolerance = 1e-05)
}

{
### remove function ####
  rm(gen.data, simu.dat, .Random.seed, tmp)
  T
}

