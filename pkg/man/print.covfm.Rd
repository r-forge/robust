\name{print.covfm}
\alias{print.cov}
\alias{print.covRob}
\alias{print.covfm}

\title{
  Print Method
}

\description{
  The generic print method for objects of class "cov", "covRob", and "covfm".
}

\usage{
\method{print}{cov}(x, ...)
\method{print}{covRob}(x, ...)
\method{print}{covfm}(x, ...)
}

\arguments{
  \item{x}{an oject of class "cov", "covRob", or "covfm".}

  \item{\dots}{additional arguments to be passed to the print functions, for example \code{digits}.}
}

\value{
  x is invisibly returned.
}

\section{Side Effects}{
  a short description of the object is displayed in the console.
}

\seealso{
  \code{\link{print}},
  \code{\link{ccov}},
  \code{\link{covRob}},
  \code{\link{fit.models}}.
}

\examples{
  data(woodmod.dat)
  woodmod.cov <- ccov(woodmod.dat)
  woodmod.covRob <- covRob(woodmod.dat)
  print(woodmod.cov)
  print(woodmod.covRob)

  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "ccov"),
                           data = woodmod.dat)
  print(woodmod.fm)
}

\keyword{methods}

