\name{distancePlot.covfm}
\alias{distancePlot.covfm}

\title{Side-by-Side Mahalanobis Distance Plot}

\description{
  Produces side-by-side plots of Mahalanobis distance computed using the location and covariance matrix estimates contained in each element of a \code{covfm} object.
}

\usage{
  distancePlot.covfm(x, level = 0.95, id.n = 3, ...)
}

\arguments{
  \item{x}{a \code{"covfm"} object.}

  \item{level}{a single numeric value between 0 and 1 giving the chi-squared percent point used to compute the outlyingness threshold.}

  \item{id.n}{a single nonnegative integer specifying the number of extreme points to label in the plot.}

  \item{\dots}{additional arguments are passed to \code{xyplot}.}
}

\value{
  the \code{trellis} object is invisibly returned.
}

\examples{
  data(woodmod.dat)
  woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covClassic"),
                         data = woodmod.dat)
  distancePlot.covfm(woodm.fm, main = "Plot Title", xlab = "x-axis label",
                     ylab = "y-axis label", pch = 4, col = "purple")
}
\keyword{hplot}


