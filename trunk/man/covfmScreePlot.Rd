\name{covfmScreePlot}
\alias{covfmScreePlot}

\title{Screeplot}

\description{
  Draws overlaid screeplots for the models in a covfm object.
}

\usage{
  covfmScreePlot(x, variables, ...)
}

\arguments{
  \item{x}{an object of class \code{"covfm"} containing 2 elements,
    typically the result of \code{\link{fit.models}(..)}.}

  \item{variables}{a vector of positive integers specifying which eigenvalues are to be drawn.  By default the 10 largest eigenvalues are plotted.}

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
  covfmScreePlot(woodmod.fm)
}

\keyword{hplot}

