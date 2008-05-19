lmRob.effvy <- function(eff, ipsi = 1)
{

##
## Computes the tuning constant for optimal weight function given eff
##

  eff.func <- function(cc, eff, ipsi=1)
    lmRob.eff0(itype=1,ta=cc,tc=cc,ipsi=ipsi)$reff-eff

  if(ipsi == 1)      ## optimal function is used
    c.inv <- c(0.2, 2.5)

  else if(ipsi == 2) ## bisquare function is used
    c.inv <- c(0.1, 30)

  else if(ipsi == 3) ## huber function is used
    c.inv <- c(0.1, 3.5)

  uniroot(eff.func, interval = c.inv, eff = eff, ipsi = ipsi)$root
}

lmRob.const <- function(eff, ipsi = 1)
{

##
## Computes the factor used for robust tau (RF) test
##

  if(ipsi == 1) {
    if(eff == 0.95) cc <- 1.060158
    else if(eff == 0.9) cc <- 0.9440982
    else if(eff == 0.85) cc <- 0.8684
    else if(eff == 0.8) cc <- 0.8097795
    else cc <- lmRob.effvy(eff)
  }
  else {
    if(eff == 0.95) cc <- 4.685061
    else if(eff == 0.9) cc <- 3.882646
    else if(eff == 0.85) cc <- 3.443689
    else if(eff == 0.8) cc <- 3.136909
    else cc <- chb(eff)$cb
  }

  tmp <- lmRob.eff0(itype = 1, ta = cc, tc = cc, ipsi = ipsi)
  tmp$alfa/tmp$beta
}

