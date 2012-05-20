\name{summary.covfm}
\alias{summary.covMLE}
\alias{summary.covRob}
\alias{summary.covfm}

\title{
  Summary Method
}

\description{
  The generic summary method for objects of class "covMLE", "covRob", and "covfm".
}

\usage{
\method{summary}{covMLE}(object, ...)
\method{summary}{covRob}(object, ...)
\method{summary}{covfm}(object, ...)
}

\arguments{
  \item{object}{an object of class "covMLE", "covRob", or "covfm".}

  \item{\dots}{additional arguments for the summary method.}
}

\value{
  an object of class "summary.covMLE", "summary.covRob", or "summary.covfm" respectively.  Objects of class "summary.cov" and "summary.covRob" have the following components.  Objects of class "summary.covfm" are lists whose elements are "summary.cov" and "summary.covRob" objects.

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the estimate of the location vector.}

  \item{evals}{a numeric vector containing the eigenvalues of the covariance/correlation matrix.}

  \item{dist}{a numeric vector containing the Mahalanobis distances.  Only present if \code{distance = TRUE} in the \code{call}.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then \code{cov} contains an estimate of the correlation matrix of \code{x}.}
}

\seealso{
  \code{\link{summary}},
  \code{\link{covMLE}},
  \code{\link{covRob}},
  \code{\link{fit.models}}.
}

\examples{
  data(woodmod.dat)
  woodmod.cov <- covMLE(woodmod.dat)
  woodmod.sc <- summary(woodmod.cov)

  woodmod.covRob <- covRob(woodmod.dat)
  woodmod.scr <- summary(woodmod.covRob)

  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "cov"),
                           data = woodmod.dat)
  woodmod.sfm <- summary(woodmod.fm)
}

\keyword{methods}

