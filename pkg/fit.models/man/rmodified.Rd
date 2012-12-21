\name{rmodified}
\alias{rmodified}
\alias{rmodified.default}

\title{
Extract Modified Residuals
}

\description{
Extract the \emph{modified residuals}
\deqn{r_{i} = e_{i} / sqrt{1 - h_{ii}}}
from a fitted model object.
}

\usage{
rmodified(object, \dots)
\method{rmodified}{default}(object, \dots)
}

\arguments{
  \item{object}{a fitted model object containing a qr component and residuals.}
  \item{\dots}{additional arguments are passed to the dispatched method.}
}

\details{
The modified residuals are rescaled to have variance \eqn{\sigma^{2}} (as opposed to the standardized residuals which have variance 1).  In the context of the fit.models package, the modified residuals are preferred since diagnostic plots of the modified residuals will emphasize differences between residual scale estimates.

The name \emph{modified residuals} comes from model-based resampling (bootstrap).  See, for example,  section 6.6 in MASS.
}

\value{
a numeric vector containing the modified residuals.
}

\references{
MASS: Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
}


\examples{
## Example from lm:
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

rmodified(lm.D9)
}

\keyword{regression}
\keyword{methods}

