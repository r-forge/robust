\name{methods.lmRob}
\alias{coef.lmRob}
\alias{deviance.lmRob}
\alias{formula.lmRob}
\alias{labels.lmRob}
\alias{model.frame.lmRob}
\alias{model.matrix.lmRob}
\alias{residuals.lmRob}
\alias{weights.lmRob}

\title{
Methods for lmRob Generic Functions
}

\description{
Accessor methods for the coefficients, deviance, formula, labels, model frame, model matrix, residuals, and weights in an \code{\link{lmRob}} object.
}

\usage{
\method{coef}{lmRob}(object, ...)
\method{deviance}{lmRob}(object, ...)
\method{formula}{lmRob}(x, ...)
\method{labels}{lmRob}(object, ...)
\method{model.frame}{lmRob}(formula, ...)
\method{model.matrix}{lmRob}(object, ...)
\method{residuals}{lmRob}(object, ...)
\method{weights}{lmRob}(object, ...)
}

\arguments{
\item{formula}{an lmRob object.}

\item{object}{an lmRob object.}

\item{x}{an lmRob object.}

\item{...}{additional arguments required by the generic functions.}
}

\value{
A vector, matrix or formula containing the requested component of the lmRob object.
}

\seealso{
\code{\link{lmRob}}, 
\code{\link{lmRob.object}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat) 
coef(stack.rob) 
deviance(stack.rob) 
formula(stack.rob) 
labels(stack.rob) 
model.frame(stack.rob) 
model.matrix(stack.rob) 
residuals(stack.rob) 
weights(stack.rob) 
}

\keyword{methods}


