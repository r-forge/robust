\name{methods.glmRob}
\alias{coef.glmRob}
\alias{family.glmRob}
\alias{labels.glmRob}
\alias{model.frame.glmRob}
\alias{model.matrix.glmRob}
\alias{residuals.glmRob}

\title{
Methods for glmRob Generic Functions
}

\description{
Accessor methods for the coefficients, deviance, formula, labels, model frame, model matrix, residuals, and weights in an \code{\link{lmRob}} object.
}

\usage{
\method{coef}{glmRob}(object, ...)
\method{family}{glmRob}(object, ...)
\method{labels}{glmRob}(object, ...)
\method{model.frame}{glmRob}(formula, ...)
\method{model.matrix}{glmRob}(object, ...)
\method{residuals}{glmRob}(object, type = c("deviance", "pearson", "working", "response"), ...)
}

\arguments{
\item{formula}{an lmRob object.}

\item{object}{an lmRob object.}

\item{type}{a character string specifying the type of residuals which should be returned.}

\item{...}{additional arguments required by the generic functions.}
}

\value{
A vector, matrix or formula containing the requested component of the glmRob object.
}

\seealso{
\code{\link{lmRob}}, 
\code{\link{lmRob.object}}.
}

\examples{
data(breslow.dat)
bres.rob <- glmRob(sumY ~ Age10 + Base4 * Trt, family = poisson(), data = breslow.dat) 
coef(bres.rob) 
family(bres.rob) 
labels(bres.rob) 
model.frame(bres.rob) 
model.matrix(bres.rob) 
residuals(bres.rob) 
}

\keyword{methods}


