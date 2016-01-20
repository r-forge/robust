\name{ddPlot.covfm}
\alias{ddPlot.covfm}

\title{Distance - Distance Plot}

\description{
  For a \code{covfm} object containing 2 models, this function plots the
  Mahalanobis distance from the first model on the y-axis and the
  Mahalanobis distance from the second model on the x-axis.
}

\usage{
ddPlot.covfm(x, level = 0.95, strip = "", id.n = 3, \dots)
}
\arguments{
  \item{x}{a \code{"covfm"} object.}
  \item{level}{a single numeric value between 0 and 1 giving the
    chi-squared percent point used to compute the outlyingness threshold.}
  \item{strip}{a character string printed in the \dQuote{strip} at the
    top of the plot.}
  \item{id.n}{a single nonnegative integer specifying the number of
    extreme points to label in the plot.}
  \item{\dots}{additional arguments are passed to \code{xyplot}.}
}

\value{
  if the models can be compared then the plotted \code{trellis} object is
  invisibly returned.  Otherwise \code{x} is returned invisibly.
}

\examples{
 data(woodmod.dat)
 woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covClassic"),
                        data = woodmod.dat)
 ddPlot.covfm(woodm.fm, main = "Plot Title", xlab = "x-axis label",
              ylab = "y-axis label", pch = 4, col = "purple")
}
\keyword{hplot}


