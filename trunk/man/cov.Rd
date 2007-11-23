\name{cov}
\alias{cov}

\title{
  Classical Covariance Estimation
}

\description{
  Computes an estimate of the covariance/correlation matrix and location vector using classical methods.
}

\usage{
  cov(data, corr = FALSE, center = TRUE, distance = TRUE, na.action = na.fail, unbiased = TRUE)
}

\arguments{
  \item{data}{a numeric matrix or data frame containing the data.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then the estimated correlation matrix is computed.}

  \item{center}{a logical flag or a numeric vector of length \code{p} (where \code{p} is the number of columns of \code{x}) specifying the center.  If \code{center = TRUE} then the center is estimated.  Otherwise the center is taken to be 0.}

  \item{distance}{a logical flag.  If \code{distance = TRUE} the Mahalanobis distances are computed.}

  \item{na.action}{a function to filter missing data.  The default \code{na.fail} produces an error if missing values are present.  An alternative is \code{na.omit} which deletes observations that contain one or more missing values.}

  \item{unbiased}{a logical flag.  If \code{unbiased = TRUE} then an unbiased estimate of the covariance matrix is computed.  If \code{unbiased = FALSE} then a maximum likelihood estimate is computed.}
}

\value{
  an object of class "\code{cov}" with components:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the estimate of the location vector.}

  \item{dist}{a numeric vector containing the Mahalanobis distances.  Only present if \code{distance = TRUE} in the \code{call}. }

  \item{corr}{a logical flag.  If \code{corr = TRUE} then \code{cov} contains an estimate of the correlation matrix of \code{x}.}
}

\details{
This function is intended to produce an object similar to that produced by the \code{covRob} in the robust library but fit using classical methods.
}

\seealso{
  \code{\link{covRob}},
  \code{\link{var}},
  \code{\link{cov.wt}}.
}

\examples{
  data(stack.dat)
  cov(stack.dat)
}

%\keyword{librobust}
\keyword{multivariate}

