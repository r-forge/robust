\name{covfmDistancePlot}
\alias{covfmDistancePlot}

\title{Distance - Distance Plot}

\description{
  Given a fit.models object containing two elements of either class "cov" or "covRob", plots the square root of the Mahalanobis distances from the first element on the y-axis and the square root of the Mahalanobis distances from the second element on the x-axis.  A 45 degree line is drawn as well.
}

\usage{
  covfmDistancePlot(x, chisq.percent = 0.975, id.n = 3, main, xlab, ylab, ...)
}

\arguments{
  \item{x}{an object of class \code{"covfm"} containing 2 elements,
    typically the result of \code{\link{fit.models}(..)}.}

  \item{chisq.percent}{a numeric value between 0 and 1 giving the chi-squared percent point used to compute the outlyingness threshold.}

  \item{id.n}{a positive integer specifying the number of extreme points to label in the plot.}

  \item{main}{a character string specifying the plot title.}

  \item{xlab}{a character string specifying the x-axis label.}

  \item{ylab}{a character string specifying the y-axis label.}

  \item{\dots}{any additional arguments are passed to \code{xyplot}.}
}

\details{
  This function is called by the generic function \code{\link{plot.covfm}}.
}

\value{
if the models can be compared then the plotted \code{trellis} object is invisibly returned, otherwise \code{x} is invisibly returned.
}

\seealso{
  \code{\link{plot.covfm}},
  \code{\link{fit.models}},
  \code{\link{covRob}},
  \code{\link{ccov}}.
}

\examples{
 data(woodmod.dat)
 woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "ccov"),
                          data = woodmod.dat)
 covfmDistancePlot(woodmod.fm)
}
\keyword{hplot}

