lmRob.genetic.control <- function(popsize=NULL, mutate.prob=NULL,
    random.n=NULL, births.n=NULL, stock=list(), maxslen=NULL,
    stockprob=NULL, nkeep=1)
{
  list(popsize=popsize, mutate.prob=mutate.prob, random.n=random.n,
       births.n=births.n, stock=stock, maxslen=maxslen,
       stockprob=stockprob, nkeep=nkeep)
}


