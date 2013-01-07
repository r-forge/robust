\name{plot.covfm}
\alias{plot.covMLE}
\alias{plot.covRob}
\alias{plot.covfm}

\title{
  Plot Method
}

\description{
  The generic plot method for objects of class "covMLE", "covRob", and "covfm".
}

\usage{
\method{plot}{covMLE}(x, which.plots = "ask", ...)
\method{plot}{covRob}(x, which.plots = "ask", ...)
\method{plot}{covfm}(x, which.plots = "ask", ...)
}

\arguments{
  \item{x}{an oject of class "cov", "covRob", or "covfm".}

  \item{which.plots}{either "ask", "all", or an integer vector specifying which plots to draw.  If which.plots is an integer vector, use the plot numbers given here (or in the "ask" menu).  The plot options are (2) Eigenvalues of Covariance Estimate, (3) Sqrt of Mahalanobis Distances, (4) Ellipses Matrix, and (5) Distance - Distance Plot.}

  \item{\dots}{additional arguments to be passed to the plotting subfunctions.}
}

\value{
  x is invisibly returned.
}

\section{Side Effects}{
  The requested plots are drawn on a graphics device.
}

\details{
The actual plot functions are only implemented for "fit.models" objects.  When this method is dispatched on an object of class "cov" or "covRob" the object is cast as a "fit.models" object containing a single element and plotted with \code{plot.covfm}.  The actual plotting is done by the subfunctions listed in the See Also section.
}

\seealso{
  \code{\link{plot}},
  \code{\link{covMLE}},
  \code{\link{covRob}},
  \code{\link{fit.models}},
  \code{\link{covfmDistancePlot}},
  \code{\link{covfmEllipsesPlot}},
  \code{\link{covfmScreePlot}},
  \code{\link{covfmSqrtMDPlot}}.
}

\examples{
data(woodmod.dat)
woodmod.cov <- covMLE(woodmod.dat)
woodmod.covRob <- covRob(woodmod.dat)
plot(woodmod.cov, which.plots = 1)
plot(woodmod.covRob, which = 1)

woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "covMLE"),
                         data = woodmod.dat)
plot(woodmod.fm, which = 1)
}

\keyword{methods}
\keyword{hplot}

