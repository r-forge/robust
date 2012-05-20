\name{lmRob.object}

\alias{lmRob.object}

\title{
  Robust Linear Model Objects 
}

\description{
These are objects of class \code{lmRob} which represent the robust fit of a linear regression model, as estimated by \code{\link{lmRob}} function. 
}

\section{Generation}{
This class of objects is returned from the \code{lmRob} function. 
}

\section{Methods}{
\code{\link{add1}}, \code{\link{anova}}, \code{\link{coef}}, \code{\link{deviance}}, \code{\link{drop1}}, \code{\link{fitted}}, \code{\link{formula}}, \code{\link{labels}}, \code{\link{plot}}, \code{\link{print}}, \code{\link{residuals}}, \code{\link{summary}}, \code{\link{update}}. 
}

\section{Structure}{
The following components must be included in a legitimate \code{"lmRob"} object: 
}
\value{

\item{coefficients}{
vector of coefficients for the robust regression. If \code{est="final"}, these are final estimates; if \code{est="initial"}, these are initial estimates. 
}

\item{T.coefficients}{
the vector of coefficients for the initial estimate, if \code{est="final"}.  
}

\item{scale}{
the scale estimate computed using the initial estimates.  
}

\item{residuals}{
the residual vector corresponding to the estimates returned in \code{coefficients}. 
}

\item{T.residuals}{
the residual vector corresponding to the estimates returned in \code{T.coefficients}. 
}

\item{fitted.values}{
the fitted values corresponding to the estimates returned in \code{coefficients}. 
}

\item{T.fitted.values}{
the fitted values corresponding to the estimates returned in \code{T.coefficients}. 
}

\item{cov}{
the estimated covariance matrix of the estimates in \code{coefficients}. 
}

\item{T.cov}{
the estimated covariance matrix of the estimates in \code{T.coefficients}. 
}

\item{rank}{
the rank of the design matrix \code{x}. 
}

\item{iter.refinement}{
the number of iterations required to refine the initial estimates.  
}

\item{df.residuals}{
the degrees of freedom in the residuals (the number of rows in \code{x} minus the rank of \code{x}). 
}

\item{est}{
a character string that specifies the type of estimates returned. If \code{est="initial"}, the initial estimates are returned; if \code{est="final"}, the final estimates are returned.  
}

\item{control}{
a list of control parameters, passed to the function \code{lmRob} as the \code{robust.control} argument that produced the \code{lmRob} object. 
}

\item{genetic.control}{
a list of control parameters, passed to the function \code{lmRob} as the \code{genetic.control} argument that produced the \code{lmRob} object, if present. 
}

\item{dev}{
the robust deviance if final MM-estimates are returned. 
}

\item{T.dev}{
the robust deviance corresponding to initial S-estimates if applies. 
}

\item{r.squared}{
the fraction of variation in \code{y} explained by the robust regression on \code{x} corresponding to the final MM-estimates in \code{coefficients}, if applies. 
}

\item{T.r.squared}{
the fraction of variation in \code{y} explained by the robust regression on \code{x} corresponding to the initial S-estimates in \code{T.coefficients}, if applies. 
}

\item{M.weights}{
the robust estimate weights corresponding to the final MM-estimates in \code{coefficients}, if applies. 
}

\item{T.M.weights}{
the robust estimate weights corresponding to the initial S-estimates in \code{T.coefficients}, if applies. 
}

\item{iter.final.coef}{
the number of iterations required to compute the final MM-estimates of the coefficients, if applies. 
}

\item{call}{
an image of the call that produced the object, but with the arguments all named and with the actual formula included as the \code{formula} argument. 
}

\item{assign}{
the same as the \code{assign} component of an \code{"lm"} object. 
}

\item{contrasts}{
the same as the \code{contrasts} component of an \code{"lm"} object. 
}

\item{terms}{
the same as the \code{terms} component of an \code{"lm"} object.  
}
}

\seealso{
\code{\link{lmRob}}. 
}

\keyword{robust}
\keyword{regression}
\keyword{methods}
\keyword{models}


