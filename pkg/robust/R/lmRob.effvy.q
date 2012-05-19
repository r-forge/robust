lmRob.effvy <- function(eff, ipsi = 1)
{
  ## Computes the tuning constant for optimal weight function
  ## given efficiency 'eff'

  stopifnot(ipsi %in% 1:3)

  eff.func <- function(cc, eff, ipsi = 1)
    lmRob.eff0(itype = 1, ta = cc, tc = cc, ipsi = ipsi)$reff - eff

  c.inv <- switch(ipsi,
            ## 1 : optimal
            c(0.2, 2.5),
            ## 2 : bisquare
            c(0.1, 30),
            ## 3 : huber
            c(0.1, 3.5)
           )

  uniroot(eff.func, interval = c.inv, eff = eff, ipsi = ipsi)$root
}


