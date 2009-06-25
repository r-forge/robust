\name{covfmEllipsesPlot}
\alias{covfmEllipsesPlot}

\title{Ellipses Plot}

\description{
  When there are 2 variables in the data this function produces a scatter plot of the data with an overlaid ellipse for each model in the covfm object.  When there are more than 2 variables this function draws a matrix with ellipses in the upper triangle.  The ellipse in the i,j cell of the matrix is drawn to be a contour of a standard bivariate normal with correlation equal to the i, j element of the correlation matrix.  There is one ellipse in each cell for each model in the covfm object.
}

\usage{
  covfmEllipsesPlot(x, ...)
}

\arguments{
  \item{x}{an object of class \code{"covfm"} containing 2 elements,
    typically the result of \code{\link{fit.models}(..)}.}

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
  \code{\link{ccov}}.
}

\examples{
  data(woodmod.dat)
  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "ccov"),
                           data = woodmod.dat)
  covfmEllipsesPlot(woodmod.fm)
}

\keyword{hplot}

