\name{print.glmRob}
\alias{print.glmRob}

\title{Print Method for glmRob Objects}

\description{
Displays the call, coefficients, degrees of freedom, and the residual deviance in a fitted glmRob object.
}

\usage{
\method{print}{glmRob}(x, ...)
}

\arguments{
  \item{x}{a glmRob object.}

  \item{\dots}{additional arguments required by the generic print function.  In particular the 'digits' argument is useful for specifying the number of significant digits when displaying numeric output.}
}

\value{
\code{x} is invisibly returned.
}

\seealso{
\code{\link{glmRob}}, 
\code{\link{glmRob.object}}.
}

\examples{
data(breslow.dat)
bres.rob <- glmRob(sumY ~ Age10 + Base4*Trt, family = poisson(), data = breslow.dat)
print(bres.rob)
print(bres.rob, digits = 3)
}

\keyword{methods}

