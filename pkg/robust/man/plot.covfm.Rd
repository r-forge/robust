\name{plot.covfm}
\alias{plot.covfm}
\alias{plot.covRob}
\alias{plot.covClassic}

\title{
  Plot Method 
}

\description{
  Generic plot method for objects with classes \dQuote{covfm}, \dQuote{covRob}, and \dQuote{covClassic}.
}

\usage{
\method{plot}{covfm}(x, which.plots = c(4, 3, 5), \dots)
\method{plot}{covRob}(x, which.plots = c(4, 3, 5), \dots)
\method{plot}{covClassic}(x, which.plots = c(4, 3, 5), \dots)
}

\arguments{
  \item{x}{an oject of class "covClassic", "covRob", or "covfm".}

  \item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  If which.plots is an integer vector, use the plot numbers given here (or in the "ask" menu).  The plot options are (2) Eigenvalues of Covariance Estimate, (3) Sqrt of Mahalanobis Distances, (4) Ellipses Matrix, and (5) Distance - Distance Plot.}

  \item{\dots}{additional arguments are passed to the plot subfunctions.}
}

\value{
  \code{x} is invisibly returned.
}

\section{Side Effects}{
  The requested plots are drawn on a graphics device.
}

\details{
The actual plot functions are only implemented for "fit.models" objects.  When this method is dispatched on an object of class "cov" or "covRob" the object is cast as a "fit.models" object containing a single element and plotted with \code{plot.covfm}.  The actual plotting is done by the subfunctions listed in the See Also section.
}

\seealso{
  \code{\link{plot}},
  \code{\link{covClassic}},
  \code{\link{covRob}},
  \code{\link{fit.models}},
  \code{\link{ddPlot.covfm}},
  \code{\link{ellipsesPlot.covfm}},
  \code{\link{screePlot.covfm}},
  \code{\link{distancePlot.covfm}}.
}

\examples{
data(woodmod.dat)

woodm.cov <- covClassic(woodmod.dat)
woodm.covRob <- covRob(woodmod.dat)

plot(woodm.cov)
plot(woodm.covRob)

woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covClassic"),
                       data = woodmod.dat)
plot(woodm.fm)
}

\keyword{methods}
\keyword{hplot}

