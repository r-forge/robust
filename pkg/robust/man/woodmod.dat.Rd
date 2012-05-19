\name{woodmod.dat}
\alias{woodmod.dat}

\title{
  Modified Wood Data
}

\description{
  The explanatory variables from the Modified Data on Wood Specific
  Gravity analyzed in Rousseeuw and Leroy (1987).

  Note that \code{data(wood, package="robustbase")} contains the same
  data, and additionally the y-variable.
}

\usage{
  data(woodmod.dat)
}

\format{
  This data frame contains the following variables:

  \describe{
    \item{V1}{number of fibers per square milimeter in Springwood (coded by dividing by 1000).}

    \item{V2}{number of fibers per square milimeter in Summerwood (coded by dividing by 10000).}

    \item{V3}{fraction of Springwood.}

    \item{V4}{fraction of light absorption by Springwood.}

    \item{V5}{fraction of light absorption by Summerwood.}

  }
}

\source{
  Rousseeuw, P. J., and Leroy, A. M. (1987). \emph{Robust Regression and Outlier Detection}. New York: Wiley.
}

\examples{
  data(woodmod.dat)
  woodmod.dat

  data(wood, package = "robustbase")
  stopifnot(data.matrix(woodmod.dat) ==
            data.matrix(wood [,1:5]))
}

\keyword{datasets}

