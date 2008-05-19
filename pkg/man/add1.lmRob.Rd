\name{add1.lmRob}
\alias{add1.lmRob}

\title{Add Terms to a Robust Linear Model Object}

\description{
\code{add1.lmRob} is used to investigate an \code{lmRob} object by adding to it, in turn, each of a number of specified terms. 
}

\usage{
\method{add1}{lmRob}(object, scope=. ~ ., scale, keep, ...)
}

\arguments{
\item{object}{an lmRob object.}

\item{scope}{a \code{formula} object describing the terms to be added. This argument is required, and is parsed to produce a set of terms that may be added to the model on their own without breaking the hierarchy rules. The \code{scope} can also be a character vector of term labels. Any \code{"."} in \code{scope} is interpreted relative to the formula implied by the \code{object} argument.}

\item{scale}{an estimate of the scale of the residuals. If not supplied, the initial estimate of the scale in \code{object} is used.}

\item{keep}{a character vector of names of components that should be saved for each augmented model. Only names from the set \code{"coefficients"}, \code{"fitted"} and \code{"residuals"} are allowed.  If \code{keep} is \code{TRUE}, the complete set is implied.  The default behavior is not to keep anything.}

\item{\dots}{additional arguments required by the generic \code{\link{add1}} function.}
}

\value{
if \code{keep} is missing, an \code{anova} object corresponding to each superset model implied by \code{object} and \code{scope}; otherwise, a list with components: 

\item{anova}{an \code{anova} object corresponding to each superset model implied by \code{object} and \code{scope}.}

\item{keep}{a matrix of mode \code{"list"}, with a column for each superset model, and a row for each component kept.}

\item{\dots}{additional arguments required by the generic add1 function.}
}

\details{
An \code{anova} object is constructed, consisting of the term labels, the degrees of freedom, and Robust Final Prediction Errors (RFPE) for each superset model.  

This function implements the generic function \code{\link{add1}} for \code{lmRob} class objects.
}

\seealso{
\code{\link{lmRob}}, 
\code{\link{add1}}, 
\code{\link{anova}}, 
\code{\link{drop1}}, 
\code{\link{lmRob.object}}.   
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ Water.Temp, data = stack.dat) 
add1(stack.rob, . ~ . + Air.Flow + Acid.Conc.) 
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


