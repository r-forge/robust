glmRob.Initial.LMS <- function(X, y, ni, dist, offset, icase)
{
  # LMS estimate using transformed y-s. #

  n <- length(y)
  np <- ncol(X)

  if(length(offset) == 0) 
    offset <- rep(0.0, n)

  # Transform the y-s #

  if(icase != 3) {
    nip1 <- ni+1
    sy <- (y+0.5)/nip1 
    ytild <- log(sy/(1-sy)) - offset
  }

  else {
    sy <- y
    sy[y <= 0.0] <- 0.5
    ytild <- log(sy) - offset
  }

  # Initial estimate of theta (LMS) #

  if(all(X[,1] == 1)) {
    if(np == 1) {
      theta0 <- median(ytild)
      sigma0 <- mad(ytild)
      return(list(theta0 = theta0, sigma0 = sigma0))
    }

    else {
      XX <- X[,-1]
      itc <- TRUE
    }
  }

  else {
    XX <- X
    itc <- FALSE
  }

  zt0 <- lmsreg(x = XX, y = ytild, intercept = itc)
  theta0 <- zt0$coef
  sigma0 <- zt0$scale
  list(theta0 = theta0, sigma0 = sigma0)
}



