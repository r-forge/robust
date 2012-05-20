lmRob.ga <- function(x, y, rk, control, ips, xk, beta, intch=1,
    tolr=1.e-3, tau=1.e-6, maxs1=50) 
{

##
## Step 0. Extract Parameters
##

  n <- length(y)
  p <- ncol(x)
  smin <- double(1)
  theta <- double(p)
  rs <- double(n)
  popsize <- control$popsize
  mutate.prob <- control$mutate.prob
  random.n <- control$random.n
  births.n <- control$births.n
  stock <- control$stock
  maxslen <- control$maxslen
  stockprob <- control$stockprob
  nkeep <- control$nkeep

  if (is.null(popsize))
		popsize <- 10*rk

  if (is.null(mutate.prob))
    mutate.prob <- c(0.15,0.2,0.2,0.2)

  else {
    if (length(mutate.prob) != 4)
      stop("mutate.prob must have length 4.")
    if (any(mutate.prob < 0))
      stop("negative value in mutate.prob.")
    if (sum(mutate.prob[2:4]) > 1)
      stop("sum of last 3 mutation probabilities greater than 1.")
  }

  if (is.null(random.n)) 
    random.n <- 50*rk

  if (is.null(births.n)) 
    births.n <- 50*rk+15*rk*rk

  if (is.null(maxslen)) {
    np2 <- trunc((n-rk)/2)
    if (np2 <= rk) 
      maxslen <- rk
    else 
      maxslen <- min(5*rk, np2)
  }

  else if (maxslen < p)
    stop("maxslen is too small.")

  if (is.null(stockprob))
    stockprob <- cumsum((2*(popsize:1))/popsize/(popsize+1))

  else {
    if (length(stockprob) != popsize)
      stop("length of stockprob must be equal to popsize.")
    if (any(stockprob < 0) || any(diff(stockprob) < 0) || 
        is.character(all.equal.numeric(stockprob[popsize],1)))
      stop("stockprob must be cumulative probabilities.")
  }

##
## Step 1. Obtain the Stock and Others
##

  noldstock <- length(stock)
  stockmat <- matrix(0,maxslen,popsize)
  stock.len <- integer(popsize)

  if (noldstock) {
    noldstock <- min(noldstock, popsize)
    for (i in 1:noldstock) {
      si <- stock[[i]]
      ll <- length(si)
      if (ll > maxslen || ll < rk)
        stop(paste("length of component", i, "of stock is not between",
            rk, "and", maxslen))
      if (any(si > n) || any(si <= 0))
        stop(paste("bad observation number(s) in component",i,"of stock."))
      stockmat[1:ll,i] <- stock[[i]]
      stock.len[i] <- ll
    }
  }

  storage.mode(x) <- "double"
  storage.mode(stockmat) <- "integer"
  xx <- matrix(0,maxslen,p)
  storage.mode(xx) <- "double"
  yy <- double(maxslen)

  z <- .Fortran("roblibgenem2",
                x,
                as.double(y),
                as.integer(n),
                as.integer(rk),
                as.integer(popsize),
                as.double(mutate.prob),
                as.integer(random.n),
                as.integer(births.n),
                stock=stockmat,
                as.integer(maxslen),
                objective=double(popsize),
                integer(2*maxslen),
                stock.len=as.integer(stock.len),
                as.integer(noldstock),
                as.double(stockprob),
                as.integer(intch),
                as.double(tolr),
                as.double(tau),
                as.integer(maxs1),
                smin=smin,
                theta=theta,
                rs=rs,
                sz=double(n),
                integer(rk),
                double(rk),
                double(rk),
                xtheta=double(maxslen),
                yy=yy,
                double(rk),
                xx=xx,
                integer(maxslen),
                ips=as.integer(ips),
                xk=as.double(xk),
                beta=as.double(beta),
                as.double(1.0))

  ord <- order(z$objective)
  nkeep <- max(1,nkeep)

  if (nkeep > popsize) 
    nkeep <- popsize

  ord <- ord[1:nkeep]
  lengths <- z$stock.len[ord]
  stockmat <- z$stock
  stock <- vector("list",nkeep)

  for (i in 1:nkeep)
    stock[[i]] <- stockmat[1:lengths[i],ord[i]]

  list(theta=z$theta, smin=z$smin, rs=z$rs, objective=z$objective[ord],
       stock=stock, births.n=births.n)
}

