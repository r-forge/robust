\name{print.summary.lmRob}
\alias{print.summary.lmRob}

\title{Print Method for summary.lmRob Objects}

\description{
Displays the call, quartiles of the residuals, an anova table for the coefficients, residual standard error, degrees of freedom, robust multiple r-squared, and the test for bias. If the summary.lmRob object contains a correlation matrix for the coefficients it will be displayed as well.
}

\usage{
\method{print}{summary.lmRob}(x, ...)
}

\arguments{
  \item{x}{a summary.lmRob object.}

  \item{\dots}{additional arguments required by the generic print function.  In particular the 'digits' argument is useful for specifying the number of significant digits when displaying numeric output.}
}

\value{
\code{x} is invisibly returned.
}

\seealso{
\code{\link{lmRob}}, 
\code{\link{lmRob.object}}.
}

\examples{
data(stack.dat)
stack.robsum <- summary(lmRob(Loss ~ ., data = stack.dat))
print(stack.robsum, digits = 4)
}

\keyword{methods}


