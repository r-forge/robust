#### Testing  glmRob()	 -*- R -*-
####
## Original
## Matias Salibian
## 01/27/2000 - Dec 05,2000

{
    gen.data.poisson <- function(coeff, family = poisson,
				 n = 100, seed = 837) {
	set.seed(seed)
	x <- cbind(rnorm(n, 1), rnorm(n, 1)^3, exp(rnorm(n, 1)))
	data.frame(y = rpois(n, lambda=exp(x %*% matrix(-coeff, ncol = 1))),
		   x1 = x[, 1], x2 = x[, 2], x3 = x[, 3], x4 = rnorm(n, 1))
    }
    is.function(gen.data.poisson)
}

{
    is.function(
    gen.data.binomial <- function(coeff, family = binomial,
				  n = 100, seed = 8373, size=1) {
	set.seed(seed)
	x <- cbind(x1 = rnorm(n, 1), x2 = runif(n), x3 = rexp(n, rate=1/3))
	tmp <- x %*% matrix(coeff, ncol = 1)
	y <- rbinom(n, size=size, p=exp(tmp)/(1+exp(tmp)) )
	if(size > 1) y <- cbind(y=y, n..y = size-y)
	data.frame(y, x, x4 = rnorm(n,1))
    }
		)
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
#	family = poisson,
#	cubif.control=glmRob.cubif.control(epsilon=1e-8, maxit = 500)))),
#	c(-0.3338872105, -1.169435637, -1.142751849,
#		-1.034934453, 0.6739420449),
#	 tolerance = 1.0e-6)
#}

### test estimates for poisson ###
{
    m <- glmRob(y~x1+x2+x3+x4, data=simu.dat.p, family = poisson,
		cubif.control=glmRob.cubif.control(epsilon=1e-10, maxit = 500))
    all(all.equal(as.vector(coef(m)),
		  c(-1.349291131, -1.61790193, -0.9282511143,
		    -0.4020939672, 0.5642934584)),
	all.equal(unname(coef(summary(m))[,"Std. Error"]),
		  c(1.679225424, 0.6048703702, 0.2509568181,
		    0.4524498791, 0.7786356952)))
}


### test estimates for binomial ###
{
    m <- glmRob(y~x1+x2+x3+x4, data=simu.dat.b,
		family = binomial,
		cubif.control=
		glmRob.cubif.control(epsilon=1e-8, maxit = 500))
    all.equal(unname(coef(m)),
	      c(-0.7348144506, 1.980241916, 0.3496657122,
		-0.429945881, -0.440491566)) &
    all.equal(unname(coef(summary(m))[,"Std. Error"]),
	      c(0.755959362, 0.40375663, 0.994048946,
		0.147288514, 0.249126759))
}


{
  class(simu.dat.b2 <- gen.data.binomial(c(2,-2,-.5), size=7)) == "data.frame"
}


### test estimates for binomial ###
{
    m <- glmRob(cbind(y, n..y) ~ x1+x2+x3+x4, data = simu.dat.b2,
		family = binomial,
		cubif.control= glmRob.cubif.control(epsilon = 1e-9))
    all.equal(unname(coef(m)),
	      c(-0.3098973658, 1.952361248, -1.389721001,
		-0.4746756755, -0.0815035817)) &
    all.equal(unname(coef(summary(m))[,"Std. Error"]),
	      c(0.296623154, 0.157524367, 0.434484679,
		0.0643572199, 0.10785387))
}


### remove function ####
{  rm(list=c(ls(patt="^gen\\.data\\."), ls(pat="^simu\\.dat\\."), "m"))
  TRUE
}
