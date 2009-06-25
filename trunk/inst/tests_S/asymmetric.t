## test script for asymmetric distribution functions -*- R -*-
## Matias Salibian
## 07/10/2000
## 09/08/2000

##
## test gammaRob and gammaMLE
##

{
  mode(
		gen.data.gamma <- function(shape, rate, n = 100, seed = 837)
		{
			set.seed(seed)
			data.frame(y = rgamma(n, shape = shape, rate = rate))
		}
  ) == "function"
}


{
	class(
		simu.dat <- gen.data.gamma(1,1,100,837)
	) == "data.frame"
}


### test estimates for tdmean estimator ###
{
	all.equal(as.vector(coef(gammaRob(data=simu.dat$y))),
		c(1.315741, 0.7474809, 1.009448), tolerance = 1.0e-6)
}


### test estimates for M estimator ###
{
	all.equal(as.vector(coef(gammaRob(data=simu.dat$y, estim = "M"))),
		c(1.0769196, 0.8915028, 0.9600768), tolerance = 1.0e-6)
}


### test estimates for MLE estimator ###
{
	all.equal(as.vector(coef(gammaMLE(data=simu.dat$y))),
		c(0.991968, 0.9892775, 0.9813317), tolerance = 1.0e-6)
}


##
## test weibullRob and weibullMLE
##


{
	mode(
		gen.data.weibull <- function(shape, scale, n = 100, seed = 837)
			{
				set.seed(seed)
				data.frame(y = rweibull(n, shape = shape, scale = scale))
			}
	) == "function"
}


{
	class(
		simu.dat <- gen.data.weibull(1,1,100,837)
	) == "data.frame"
}


### test estimates for tdmean estimator ###
{
	all.equal(as.vector(coef(weibullRob(data=simu.dat$y))),
		c(1.076855, 1.058599, 1.009448), tolerance = 1.0e-6)
}


### test estimates for M estimator ###
{
	all.equal(as.vector(coef(weibullRob(data=simu.dat$y, estim = "M"))),
		c(1.0457520, 0.9735536, 0.9563069), tolerance = 1.0e-6)
}


### test estimates for MLE estimator ###
{
	all.equal(as.vector(coef(weibullMLE(data=simu.dat$y))),
		c(1.0073973, 0.9842765, 0.9812426), tolerance = 1.0e-6)
}


##
## test lognormRob and lognormMLE
##


{
  mode(
		gen.data.lognorm <- function(meanlog, sdlog, n = 100, seed = 837)
		{
			set.seed(seed)
			data.frame(y = rlnorm(n, meanlog = meanlog, sdlog = sdlog))
		}
  ) == "function"
}


{
	class(
		simu.dat <- gen.data.lognorm(1,1,100,837)
	) == "data.frame"
}

### test estimates for tdmean estimator ###
{
	all.equal(as.vector(coef(lognormRob(data=simu.dat$y))),
		c(0.8158365, 0.9554397, 3.5261173), tolerance = 1.0e-6)
}


### test estimates for MLE estimator ###
{
	all.equal(as.vector(coef(lognormMLE(data=simu.dat$y))),
		c(0.845205, 0.933777, 3.600867), tolerance = 1.0e-6)
}


### remove function ####
{
	rm(gen.data.gamma, gen.data.weibull, gen.data.lognorm, simu.dat, .Random.seed)
	T
}

