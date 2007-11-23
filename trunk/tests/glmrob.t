##
## test script for glmRob()
## Matias Salibian
## 01/27/2000 - Dec 05,2000

{
  mode(gen.data.poisson <- function(coeff, family = poisson,
  					n = 100, seed = 837) 
       {
         set.seed(seed)
         x <- cbind(rnorm(n, 1), rnorm(n, 1)^3, exp(rnorm(n, 1)))
         data.frame(y = rpois(n, lambda=exp(x %*% matrix(-coeff, ncol = 1))), 
                    x1 = x[, 1], x2 = x[, 2], x3 = x[, 3], x4 = rnorm(n, 1))
       }
  ) == "function"
}

{
  mode(gen.data.binomial <- function(coeff, family = binomial,
  					n = 100, seed = 8373, size=1) 
       {
         set.seed(seed)
         x <- cbind(rnorm(n, 1), runif(n), rexp(n, rate=1/3))
		  tmp <- x %*% matrix(coeff, ncol = 1)
		  y <- rbinom(n, size=size, p=exp(tmp)/(1+exp(tmp)) )
		  if(size > 1) y <- cbind(y, size-y)
         data.frame(y =y,
                    x1 = x[, 1], x2 = x[, 2], x3 = x[, 3], x4 = rnorm(n, 1))
       }
  ) == "function"
}


{	
  class(simu.dat.p <- gen.data.poisson(c(1,1,1))) == "data.frame"
}
	
{	
  class(simu.dat.b <- gen.data.binomial(c(2,-2,-.5))) == "data.frame"
}
	

### test estimates for poisson ###
#{
#  all.equal(as.vector(coef(glmRob(y~x1+x2+x3+x4, data=simu.dat.p, 
# 	family = poisson, 
#	cubif.control=glmRob.cubif.control(epsilon=1e-8, maxit = 500)))),
#	c(-0.3338872105, -1.169435637, -1.142751849, 
#		-1.034934453, 0.6739420449),
#        tolerance = 1.0e-6) 
#}

### test estimates for poisson ###
{
  all.equal(as.vector(coef(glmRob(y~x1+x2+x3+x4, data=simu.dat.p, 
 	family = poisson, 
	cubif.control=glmRob.cubif.control(epsilon=1e-8, maxit = 500)))),
	c(-0.2903191, -1.126334, -1.1971, -1.02586, 0.439577),
  tolerance = 1.0e-3) 
}


### test estimates for binomial ###
{
  all.equal(as.vector(coef(glmRob(y~x1+x2+x3+x4, data=simu.dat.b, 
  	family = binomial,
	cubif.control=
		glmRob.cubif.control(epsilon=1e-8, maxit = 500)))),
	c(0.519699, 1.82827, -2.863848, -0.3784683, -0.1307886),
        tolerance = 1.0e-6) 
}


{	
  class(simu.dat.b <- gen.data.binomial(c(2,-2,-.5), size=7)) == "data.frame"
}


### test estimates for binomial ###
{
  all.equal(as.vector(coef(glmRob(cbind(y.y, y.2)~x1+x2+x3+x4, data=simu.dat.b, 
  	family = binomial,
	cubif.control=glmRob.cubif.control(epsilon=1e-8)))),
	c(0.2839545, 2.130949, -2.560649, -0.6184108, 0.0185579),
        tolerance = 1.0e-6) 
}


### remove function ####
{
  rm(gen.data.poisson, gen.data.binomial, simu.dat.b, simu.dat.p, .Random.seed)
  T
}


