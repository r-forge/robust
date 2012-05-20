lmRob.effad <- function(eff)
{

##
## Computes the cutoff value for adaptive estimator given eff
##

  eff.func <- function(cc, eff)
    -0.531700164 + 0.785133187*cc - 0.103931349*cc^2 + 0.000637741*cc^3 - eff

  uniroot(eff.func, interval = c(2.0, 3.9), eff = eff)$root
}


