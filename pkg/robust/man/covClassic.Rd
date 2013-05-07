%% --- NOTA BENE:  Renamed function  from  cov() to ccov() to covClassic()
\name{covClassic}
\alias{covClassic}

\title{Classical Covariance Estimation}

\description{
  Compute an estimate of the covariance/correlation matrix and location
  vector using classical methods.

  Its main intention is to return an object compatible to that
  produced by \code{\link{covRob}}, but fit using classical methods.
}

\usage{
covClassic(data, corr = FALSE, center = TRUE, distance = TRUE,
           na.action = na.fail, unbiased = TRUE, ...)
}

\arguments{
  \item{data}{a numeric matrix or data frame containing the data.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then the estimated correlation matrix is computed.}

  \item{center}{a logical flag or a numeric vector of length \code{p} (where \code{p} is the number of columns of \code{x}) specifying the center.  If \code{center = TRUE} then the center is estimated.  Otherwise the center is taken to be 0.}

  \item{distance}{a logical flag.  If \code{distance = TRUE} the Mahalanobis distances are computed.}

  \item{na.action}{a function to filter missing data.  The default \code{na.fail} produces an error if missing values are present.  An alternative is \code{na.omit} which deletes observations that contain one or more missing values.}

  \item{unbiased}{logical indicating if an unbiased estimate of the
    covariance matrix is should becomputed.  If false, the maximum
    likelihood estimate is computed.}
  \item{...}{additional .}
}

\value{
  a list with class \dQuote{covClassic} containing the following elements:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the estimate of the location vector.}

  \item{dist}{a numeric vector containing the squared Mahalanobis distances.  Only
    present if \code{distance = TRUE} in the \code{call}.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then \code{cov}
    contains an estimate of the correlation matrix of \code{x}.}
}

\note{
  Originally, and in S-PLUS, this function was called \code{cov}; it has
  been renamed, as that did mask the function in the standard package
  \pkg{stats}.
}

\seealso{
  \code{\link{covRob}},
  \code{\link{var}},
  \code{\link{cov.wt}}.
}

\examples{
  data(stack.dat)
  covClassic(stack.dat)
}

\keyword{robust}
\keyword{multivariate}

