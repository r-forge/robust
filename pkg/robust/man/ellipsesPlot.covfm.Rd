\name{ellipsesPlot.covfm}
\alias{ellipsesPlot.covfm}

\title{Ellipses Plot - Visual Correlation Matrix Comparison}

\description{
  When there are 3 or more variables in the data, this function produces a matrix with ellipses drawn in the upper triangle.  The ellipse in cell \eqn{i,j} of the plot is drawn to be a contour of a standard bivariate normal with correlation \eqn{\rho_{ij}}.  One ellipse is drawn in each cell for each model in the \code{covfm} object.  When there are 2 variables in the data, this function produces a scatter plot of the data with an overlaid 95\% confidence ellipse for each model in the \code{covfm} object.
}

\usage{
  ellipsesPlot.covfm(x, ...)
}

\arguments{
  \item{x}{a \code{"covfm"} object.}

  \item{\dots}{additional arguments are ignored.}
}

\value{
  x is invisibly returned.
}

\examples{
  data(woodmod.dat)
  woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covClassic"),
                         data = woodmod.dat)
  ellipsesPlot.covfm(woodm.fm)
}

\keyword{hplot}


