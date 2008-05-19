\name{print.summary.glmRob}
\alias{print.summary.glmRob}

\title{Print Method for summary.glmRob Objects}

\description{
Displays the call, quartiles of the deviance residuals, an anova table for the coefficients, the dispersion parameter, null deviance, residual deviance, and degrees of freedom. If the summary.glmRob object contains a correlation matrix for the coefficients it will be displayed as well.
}

\usage{
\method{print}{summary.glmRob}(x, ...)
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
bres.sum <- summary(bres.rob)
print(bres.sum)
print(bres.sum, digits = 4)
}

\keyword{methods}


