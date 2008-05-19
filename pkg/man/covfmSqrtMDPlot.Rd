\name{covfmSqrtMDPlot}
\alias{covfmSqrtMDPlot}

\title{Square Root of Mahalanobis Distance Plot}

\description{
  Produces side-by-side plots of the Mahalanobis distances computed using the location and covariance matrix estimates contained in each model of a covfm object.
}

\usage{
  covfmSqrtMDPlot(x, chisq.percent = 0.975, id.n = 3, ...)
}

\arguments{
  \item{x}{an object of class "covfm" containing 2 elements.}

  \item{chisq.percent}{a numeric value between 0 and 1 giving the chi-squared percent point used to compute the outlyingness threshold.}

  \item{id.n}{a positive integer specifying the number of extreme points to label in the plot.}

  \item{\dots}{additional arguments are passed to the low-level plotting functions.}
}

\details{
  This function is called by the generic function \code{\link{plot.covfm}}.  To use this function on a "cov" or "covRob" object first use \code{\link{fit.models}} to coerce the object to class "covfm".
}

\value{
  x is invisibly returned.
}

\seealso{
  \code{\link{plot.covfm}},
  \code{\link{fit.models}},
  \code{\link{covRob}},
  \code{\link{cov}}.
}

\examples{
  data(woodmod.dat)
  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "cov"), data = woodmod.dat)
  covfmSqrtMDPlot(woodmod.fm)
}

\keyword{hplot}
