\name{leverage}
\alias{leverage}
\alias{leverage.default}
\alias{leverage.lm}

\title{Leverage}

\description{
Returns a leverage measure for each sample point in the data.
}

\usage{
leverage(object, \dots)
\method{leverage}{default}(object, \dots)
\method{leverage}{lm}(object, \dots)
}

\arguments{
  \item{object}{a fitted model object containing a model frame component.}
  \item{\dots}{additional arguments are passed to the dispatched method.}
}

\value{
a numeric vactor containing the leverages.
}

\details{
For least-squares linear models the leverages are the diagonal elements of the hat matrix.  This function is generic.  The default method assigns a leverage of zero to every sample point.
}

\examples{
## Example from lm:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

leverage(lm.D9)
}

\keyword{regression}
\keyword{methods}
