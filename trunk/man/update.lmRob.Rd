\name{update.lmRob}
\alias{update.lmRob}

\title{Update an lmRob Model Object}

\description{
This is a method for the function \code{\link{update}} for objects inheriting from class \code{lmRob}.  See \code{\link{update}} for the general behavior of this function and for the interpretation of the arguments. 
}

\usage{
\method{update}{lmRob}(object, formula, evaluate = TRUE, class, ...)
}

\arguments{
\item{object}{an lmRob object.}

\item{formula}{a modeling formula, such as \code{y ~ a + b}.  A single dot \code{.} on either side of the \code{~} gets replaced by the left or right side of the formula in \code{object}.  The dot on the left can be omitted.  By default, it refits object using the same formula as in \code{object}.}

\item{evaluate}{a logical value.  If \code{TRUE} the updated call is evaluated and returned.  Otherwise the unevaluated call is returned.}

\item{class}{a single character value specifying the fitting class for the new object.}

\item{\dots}{additional arguments passed to the generic update function.}
}

\value{
either a new updated object, or else an unevaluated expression for creating such an object.
}

\details{
If \code{formula} is missing, \code{update.lmRob} alternates between the initial estimates and final estimates.  If \code{formula} is present, \code{update.lmRob} functions just like \code{update.default}.  
}

\seealso{
\code{\link{lmRob}}. 
\code{\link{update}}.
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


