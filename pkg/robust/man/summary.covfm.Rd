\name{summary.covfm}
\alias{summary.covClassic}
\alias{summary.covRob}
\alias{summary.covfm}

\title{
  Summary Method
}

\description{
  The generic summary method for objects of class "covClassic", "covRob", and "covfm".
}

\usage{
\method{summary}{covClassic}(object, ...)
\method{summary}{covRob}(object, ...)
\method{summary}{covfm}(object, ...)
}

\arguments{
  \item{object}{an object of class "covClassic", "covRob", or "covfm".}

  \item{\dots}{additional arguments for the summary method.}
}

\value{
  an object of class "summary.covClassic", "summary.covRob", or "summary.covfm" respectively.  Objects of class "summary.cov" and "summary.covRob" have the following components.  Objects of class "summary.covfm" are lists whose elements are "summary.cov" and "summary.covRob" objects.

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the estimate of the location vector.}

  \item{evals}{a numeric vector containing the eigenvalues of the covariance/correlation matrix.}

  \item{dist}{a numeric vector containing the Mahalanobis distances.  Only present if \code{distance = TRUE} in the \code{call}.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then \code{cov} contains an estimate of the correlation matrix of \code{x}.}
}

\seealso{
  \code{\link{summary}},
  \code{\link{covClassic}},
  \code{\link{covRob}},
  \code{\link{fit.models}}.
}

\examples{
  data(woodmod.dat)
  woodm.cov <- covClassic(woodmod.dat)
  summary(woodm.cov)

  woodm.covRob <- covRob(woodmod.dat)
  summary(woodm.covRob)

  woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covClassic"),
                         data = woodmod.dat)
  summary(woodm.fm)
}

\keyword{methods}

