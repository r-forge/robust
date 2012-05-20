\name{drop1.lmRob}
\alias{drop1.lmRob}

\title{Compute an Anova Object by Dropping Terms}

\description{
\code{drop1.lmRob} is used to investigate a robust Linear Model object by  
recomputing it, successively omitting each of a number of specified terms. 
}

\usage{
\method{drop1}{lmRob}(object, scope, scale, keep, fast = FALSE, ...)
}

\arguments{
\item{object}{an lmRob object.}

\item{scope}{an optional \code{formula} object describing the terms to be dropped.  Typically this argument is omitted, in which case all possible terms are dropped (without breaking hierarchy rules).  The \code{scope} can also be a character vector of term labels.  If the argument is supplied as a formula, any \code{.} is interpreted relative to the formula implied by the \code{object} argument.}

\item{scale}{a single numeric value containing a residual scale estimate.  If missing, the scale estimate in \code{object} is used.}

\item{keep}{a character vector of names of components that should be saved for each subset model.  Only names from the set \code{"coefficients"}, \code{"fitted"} and \code{"residuals"} are allowed.  If \code{keep == TRUE}, the complete set is saved.  The default behavior is not to keep anything.}

\item{fast}{a logical value.  If \code{TRUE} the robust initial estimate (used when fitting each of the reduced models) is replaced by a weighted least squares estimate using the robust weights in \code{object}.}

\item{\dots}{additional arguments required by the generic drop1 function.}
}

\value{
An \code{anova} object is constructed, consisting of the term labels, the degrees of freedom, and Robust Final Prediction Errors (RFPE) for each subset model.  If \code{keep} is missing, the \code{anova} object is returned.  If \code{keep} is present, a list with components \code{"anova"} and \code{"keep"} is returned.  In this case, the \code{"keep"} component is a matrix of mode \code{"list"}, with a column for each subset model, and a row for each component kept.
}

\details{
This function is a method for the generic function \code{\link{drop1}} for class \code{"lmRob"}.
}

\seealso{
\code{\link{anova}},
\code{\link{drop1}},
\code{\link{lmRob.object}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat) 
drop1(stack.rob) 
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


