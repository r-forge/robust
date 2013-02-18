\name{screePlot.covfm}
\alias{screePlot.covfm}

\title{Comparison Screeplot}

\description{
  Draws overlaid screeplots for the models in a \code{covfm} object.
}

\usage{
  screePlot.covfm(x, npcs, ...)
}

\arguments{
  \item{x}{a \code{"covfm"} object.}

  \item{npcs}{a postive integer value specifying the number of components to be plotted.}

  \item{\dots}{additional arguments are passed to \code{xyplot}.}
}

\value{
  the \code{trellis} object is invisibly returned.
}

\examples{
  data(woodmod.dat)
  woodm.fm <- fit.models(list(Robust = "covRob", Classical = "covMLE"),
                         data = woodmod.dat)
  screePlot.covfm(woodm.fm, main = "Plot Title", xlab = "x-axis label",
                  ylab = "y-axis label", pch = 4:5)
}

\keyword{hplot}


