\name{stack.dat}
\alias{stack.dat}

\title{
  Brownlee's Stack-Loss Data
}

\description{
  These data are from the operation of a plant for the oxidation of ammonia to nitric acid, measured on 21 consecutive days.
}

\usage{
  data(stack.dat)
}

\format{
  This data frame contains the following variables:

  \describe{
    \item{Loss}{the percentage of ammonia lost (times 10).}

    \item{Air.Flow}{air flow into the plant}

    \item{Water.Temp}{cooling water inlet temperature.}

    \item{Acid.Conc.}{acid concentration as a percentage (coded by subtracting 50 and then multiplying by 10).}
  }
}

\source{
  Brownlee, K.A. (1965).  \emph{Statistical Theory and Methodology in Science and Engineering}. New York: John Wiley & Sons, Inc.
}

\examples{
  data(stack.dat)
  stack.dat
}

\keyword{datasets}

