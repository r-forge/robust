lmRob.const <- function(eff, ipsi = 1)
{

##
## Computes the factor used for robust tau (RF) test
##

  ## FIXME:  support for 'huber'  ?
  stopifnot(ipsi %in% c(1,2))

  cc <-
      if(ipsi == 1) {
          if     (eff == 0.95) 1.060158
          else if(eff == 0.9)  0.9440982
          else if(eff == 0.85) 0.8684
          else if(eff == 0.8)  0.8097795
          else  lmRob.effvy(eff, ipsi=ipsi)
      }
      else { ## ipsi = 2
          if(eff == 0.95)  4.685061
          else if(eff == 0.9)  3.882646
          else if(eff == 0.85)  3.443689
          else if(eff == 0.8)  3.136909
          ## else  chb(eff)$cb
          else  lmRob.effvy(eff, ipsi=ipsi)
      }

   tmp <- lmRob.eff0(itype = 1, ta = cc, tc = cc, ipsi = ipsi)
   tmp$alfa / tmp$beta
}


