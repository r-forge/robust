\name{overlaidDenPlot.fdfm}
\title{Overlaid Density Plot}
\alias{overlaidDenPlot.fdfm}
\description{
  Plot the estimated densities over a histogram of the data.
}

\usage{
overlaidDenPlot.fdfm(x, trunc = 1.0 - 1e-3, \dots)
}
\arguments{
  \item{x}{an \code{fdfm} object.}
  \item{trunc}{if non NULL, the maximum x-value of the plot is the
    largest \code{truncate} quantile among the estimated distributions.}
  \item{\dots}{additional arguments are passed to the plotting functions.}
}
\value{
  \code{x} is invisibly returned.
}

\examples{
  data(los, package="robustbase")
  los.fm <- fit.models(c(Robust = "fitdstnRob", MLE = "fitdstn"),
                         x = los, densfun = "gamma")
  overlaidDenPlot.fdfm(los.fm, xlab = "x-axis label", ylab = "y-axis label",
                       main = "Plot Title")
}
\keyword{hplot}


