\name{print.lmRob}
\alias{print.lmRob}

\title{Print Method for lmRob Objects}

\description{
Displays the call, coefficients, degrees of freedom, and the residual standard error in a fitted lmRob object.
}

\usage{
\method{print}{lmRob}(x, ...)
}

\arguments{
\item{x}{an lmRob object.}

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
stack.rob <- lmRob(Loss ~ ., data = stack.dat)
print(stack.rob)
print(stack.rob, digits = 3)
}

\keyword{methods}


