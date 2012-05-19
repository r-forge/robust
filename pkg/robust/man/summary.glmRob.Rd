\name{summary.glmRob}
\alias{summary.glmRob}

\title{Summarizing Robust Generalized Linear Model Fits}

\description{
Compute a summary of the robustly fitted generalized linear model.
}

\usage{
\method{summary}{glmRob}(object, correlation = TRUE, ...)
}

\arguments{
  \item{object}{a glmRob object.}

  \item{correlation}{a logical value. If \code{TRUE} then the correlation matrix of the coefficients is included in the summary.}

  \item{\dots}{additional arguments required by the generic \code{\link{summary}} function.}
}

\value{
The summary is returned in a list of class summary.glmRob and contains the following components:
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}

\examples{
data(breslow.dat)
bres.rob <- glmRob(sumY ~ Age10 + Base4*Trt, family = poisson(), data = breslow.dat)
bres.sum <- summary(bres.rob)
bres.sum
}

\keyword{methods}
\keyword{robust}
\keyword{regression}

