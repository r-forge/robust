## test script for discRob -*- R -*-
## Matias Salibian
## Dec 2000
{
    set.seed(99)
    tmp.hemo.rob <- discRob(Group ~., data = hemo.cont, family = Classical('hetero'))
    0.31929799 - sum( sqrt(unlist(lapply(tmp.hemo.rob$covariances, sum)))) < 1e-6
}

{
    rm(tmp.hemo.rob)
    TRUE
}
