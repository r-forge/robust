lmRob.const <- function(eff, ipsi = 1)
{

##
## Computes the factor used for robust tau (RF) test
##

  ## FIXME:  support for 'huber'  ?
  stopifnot(ipsi %in% c(1, 2))


  idx <- match(eff, c(0.8, 0.85, 0.9, 0.95), nomatch = -1)

  if(ipsi == 1) {
    if(idx > 0)
      cc <- c(0.8097795, 0.8684, 0.9440982, 1.060158)[idx]
    else
      cc <- lmRob.effvy(eff, ipsi = ipsi)
  }

  if(ipsi == 2) {
    if(idx > 0)
      cc <- c(3.136909, 3.443689, 3.882646, 4.685061)[idx]
    else
      cc <- lmRob.effvy(eff, ipsi = ipsi)
  }

   tmp <- lmRob.eff0(itype = 1, ta = cc, tc = cc, ipsi = ipsi)
   tmp$alfa / tmp$beta
}


