\name{lmRob.effvy}
\alias{lmRob.effvy}

\title{
  Constant for the Optimal Loss (Weight) Function 
}

\description{
Returns the constant of the optimal loss (weight) function, given the asymptotic efficiency under Gaussian model.  
}

\usage{
  lmRob.effvy(eff, ipsi = 1)
}

\arguments{
  \item{eff}{a numeric value between 0 and 1 specifying the desired asymptotic efficiency.}

  \item{ipsi}{choose your weight function: \code{ipsi = 1} for optimal function, \code{ipsi = 2} for rescaled bisquare function, and \code{ipsi = 3} for Huber function.}
}

\value{
the constant of the optimal loss (weight) function which will give the required asymptotic efficiency. 
}

\seealso{
\code{\link{lmRob}}. 
}

\keyword{robust}


