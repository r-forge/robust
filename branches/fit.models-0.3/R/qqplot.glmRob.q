qqplot.glmRob <- function(y, par, family)
{

## First some helper functions ##

  glmRob.binom.dev <- function(p)
  {
    # this function computes the posibles desviances and its probabilities
    # when the responses  have distribution binomial with parameter p
    # The output is out: first element are the  the values  of the deviances 
    # (there may be repeated elements) and the probabilities the second element.

    p <- pmax(p, .000000000001)
    p <- pmin(p, .999999999999)
    n <- length(p)
    z1 <- sqrt(-log(p))
    u1 <- p/n
    z2 <- sqrt(-log(1-p))
    u2 <- (1-p)/n
    d <- sqrt(2)*c(z1,-z2)
    p <- c(u1,u2)
    list(d,p)
  }

  glmRob.poiss.dev <- function(lan)
  {
    # this function computes the posibles desviances and its probabilities
    # when the responses have distribution Poisson with parameter lan
    # The output is out: first element are the the values of the deviances 
    # (there may be repeated elements) and the probabilities the second element.
    # Warning if some elements of lan are very large the computing time may
    # be very large

    lan <- pmax(lan, 0.000000000001)
    n <- length(lan)
    qp1 <- qpois(.0001, lan)
    qp1 <- pmax(qp1-1, 0)
    qp11 <- pmax(qp1, 1)
    qp2 <- qpois(.9999, lan) + 1

    # qp[i] gives the number of values of the Poisson distribution that
    # will be consider for the i value of the parameter

    y1 <- qp11[1]:qp2[1]

    # In d we acumulate all he values of the deviances
    # In pp we acumulate all the probabilities

    d <- lan[1]-(log(lan[1])*y1)-y1+(y1*log(y1))
    d <- sqrt(d)*sign(y1-lan[1])

    if(qp1[1] > 0.0)
      pp <- dpois(y1,lan[1])

    if(qp1[1] == 0.0) {
      d <- c(-sqrt(lan[1]), d)
      pp <- dpois(c(0,y1), lan[1])
    } 

    for(i in 2:n) {
      y1 <- qp11[i]:qp2[i]
      d1 <- lan[i] - (log(lan[i])*y1) - y1 + (y1*log(y1))
      d1 <- sqrt(d1) * sign(y1-lan[i])

     if(qp1[i] > 0.0)
       pp1 <- dpois(y1, lan[i])

      if(qp1[i] == 0.0) {      
        d1 <- c(-sqrt(lan[i]), d1)
        pp1 <- dpois(c(0,y1), lan[i])
      }

      d <- c(d, d1)
      pp <- c(pp, pp1)
    }

    d <- sqrt(2)*d
    ppp <- sum(pp)
    pp <- pp/ppp
    list(d, pp)
  }

  glmRob.group.values <- function(x, p)
  {
    # given a vector x of possible no ordered values (may be repeated values)
    # with probabilities p the function group 
    # the  same values adding their probabilities
    # the output is out with first element the ordered values (all
    # differents) and second element
    # the probabilities

    n <- length(x)
    xxx <- double(n)
    pp <- double(n)
    xx <- sort(x)
    u <- order(x)
    po <- p[u]
    u <- diff(xx)
    v <- abs(u) > 0.0000000001
    r <- 1:(n-1)
    vv <- r[v]
    h <- length(vv) + 1
    xxx[1] <- xx[1]
    pp[1] <- sum(po[1:vv[1]])

    for(i in 2:(h-1)) {
      l <- vv[i-1] + 1
      xxx[i] <- xx[l]
      pp[i] <- sum(po[(vv[i-1]+1):vv[i]])
    }

    pp[h] <- sum(po[(vv[h-1]+1):n])
    xxx[h] <- xx[n]
    list(xxx[1:h], pp[1:h])
  }

  glmRob.quant <- function(x, p, n)
  {
    # Given a discrete distribution x the values (ordered and different)
    # with probabilities p, this subroutine compute the quantiles 
    # (i-.5)/n, i=1,...,n

    z <- double(n)
    pp <- double(n)
    xx <- sort(x)
    u <- order(x)
    po <- p[u]
    j <- 0
    aa <- 0

    for(i in 1:n) {
      hh <- 0
      while(hh == 0) {
        j <- (j+1)
        pp[j] <- aa + po[j]
        aa <- pp[j]
        hh <- (pp[j] >= ((i - 0.5)/n))
      }

      z[i]<-xx[j]
    }

    z
  }


  ## Here is the qqplot.glmRob function ##

  # y is the response variable, 
  # par the parameter values
  # dist=0 binomial, dist=1 Poisson
  # the output are the quantiles (out$quantiles) and 
  # the deviances residuals (out$deviance.residuals)

  n <- length(y)
  dev <- double(n)

  if(family == "binomial") {
    par <- pmax(par,.000000000001)
    par <- pmin(par,.999999999999)
    uu <- glmRob.binom.dev(par)
    dev <- y * sqrt(-log(par)) - (1 - y) * sqrt(-log(1 - par))
    dev <- sort(dev)
  }

  if(family == "poisson") {
    par <- pmax(par, .000000000001)
    uu <- glmRob.poiss.dev(par)
    v1 <- (y == 0)
    v2 <- (y > 0)
    v3 <- 1:n
    v1 <- v3[v1]
    v2 <- v3[v2]
    dev[v1] <- -sqrt(par[v1])
    dev[v2] <- par[v2] - y[v2] * log(par[v2]) - y[v2] + y[v2] * log(y[v2])
    dev[v2] <- sqrt(dev[v2]) * sign((y[v2]-par[v2]))
    dev <- sort(dev)
  }

  uu <- glmRob.group.values(uu[[1]], uu[[2]])

  list(quantiles = glmRob.quant(uu[[1]], uu[[2]], n),
       deviance.residuals = sqrt(2)*dev)
}
 
