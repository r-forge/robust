\name{predict.lmRob}
\alias{predict.lmRob}

\title{Use predict() on an lmRob Object }

\description{
Extracts the fitted values from an \code{lmRob} object and returns a matrix of predictions. 
}

\usage{
\method{predict}{lmRob}(object, newdata, type = "response", se.fit = FALSE, terms = labels(object), ...)
}

\arguments{
\item{object}{an lmRob object.}

\item{newdata}{a data frame containing the values at which predictions are required.  This argument can be missing, in which case predictions are made at the same values used to compute the object.  Only those predictors referred to in the right side of the formula in object need be present by name in \code{newdata}.}

\item{type}{a single character value specifying the type of prediction.  The only choice is "response".  If "response" is selected, the predictions are on the scale of the response.}

\item{se.fit}{a logical value.  If \code{TRUE}, pointwise standard errors are computed along with the predictions.}

\item{terms}{this argument is presently unused.}

\item{\dots}{additional arguments required by the generic \code{\link{predict}} function.}
}

\value{
a vector of predictions, or a list consisting of the predictions and their standard errors if \code{se.fit = TRUE}.
}

\section{Warning}{\code{predict} can produce incorrect predictions when the \code{newdata} argument is used if the formula in \code{object} involves \emph{data-dependent} transformations, such as \code{poly(Age, 3)} or \code{sqrt(Age - min(Age))}.}

\seealso{
\code{\link{lmRob}},
\code{\link{predict}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat)
predict(stack.rob)
predict(stack.rob, newdata = stack.dat[c(1,2,4,21), ], se.fit = TRUE)
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


