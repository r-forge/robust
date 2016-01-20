\name{leverage}
\alias{leverage}
\alias{leverage.default}
\alias{leverage.lm}

\title{Leverage Measure for Each Data Point}

\description{
Returns a leverage measure for each sample point in the data.
}

\usage{
leverage(object, \dots)
\method{leverage}{default}(object, \dots)
\method{leverage}{lm}(object, \dots)
}

\arguments{
  \item{object}{a fitted model object.}
  \item{\dots}{additional arguments are passed to the dispatched method.}
}

\value{
a numeric vector containing the leverages.
}

\details{
  For least-squares linear models the leverages are the diagonal elements
  of the hat matrix.  This function is generic.   The default method
  assigns a leverage of \eqn{p/n} to every sample point.
}
\seealso{
  \R's traditional function with the identical semantic:
  \code{\link{hatvalues}} (which is also an S3 generic from standard
  package \pkg{stats}).
%% and also has an 'lm' method which is slightly better than this one !
%% MM: Do we *really* need this ???  I propose to deprecate it (and
%%     later make it defunct).
}

\examples{
require("stats")
## Example from lm:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

## balanced design --> constant leverages :
leverage(lm.D9)
hatvalues(lm.D9)
}

\keyword{regression}
\keyword{methods}
