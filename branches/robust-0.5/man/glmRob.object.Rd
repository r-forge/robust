\name{glmRob.object}
\alias{glmRob.object}

\title{Robust Generalized Linear Model Fit}

\description{These are objects of class \code{glmRob} which represent the robust fit of a generalized linear regression model, as estimated by \code{glmRob()}.}

\section{Methods}{
\code{anova},
\code{coefficients},
\code{deviance},
\code{fitted.values},
\code{family}, \code{formula},
\code{plot}, \code{print},
\code{residuals},
\code{summary}. 
}

\section{Structure}{
The following components must be included in a legitimate
\code{"glmRob"} object. Residuals, fitted values, and
coefficients should be extracted by the generic functions of the same name,
rather than by the \code{"\$"} operator. The
\code{family} function returns the entire family
object used in the fitting, and \code{deviance} can
be used to extract the deviance of the fit. 
}

\value{

\item{coefficients}{
the coefficients of the \code{linear.predictors},
which multiply the columns of the model matrix. The names of the
coefficients are the names of the single-degree-of-freedom effects (the
columns of the model matrix). If the model is over-determined there will be
missing values in the coefficients corresponding to inestimable
coefficients.
}
\item{linear.predictors}{
the linear fit, given by the product of the model matrix and the
coefficients.
}
\item{fitted.values}{
the fitted mean values, obtained by transforming
\code{linear.predictors} using the inverse link
function. 
}
\item{residuals}{
the residuals from the final fit; also known as working residuals, they are
typically not interpretable.
}
\item{deviance}{
up to a constant, minus twice the log-likelihood evaluated at the final
\code{coefficients}. Similar to the residual sum of
squares.
}
\item{null.deviance}{
the deviance corresponding to the model with no predictors. 
}
\item{family}{
a 3 element character vector giving the name of the family, the link and
the variance function.
}
\item{rank}{
the number of linearly independent columns in the model matrix.
}
\item{df.residuals}{
the number of degrees of freedom of the residuals.
}
\item{call}{
a copy of the call that produced the object.
}
\item{assign}{
the same as the \code{assign} component of an
\code{"lm"} object.
}
\item{contrasts}{
the same as the \code{contrasts} component of an \code{"lm"} object.  
}
\item{terms}{
the same as the \code{terms} component of an \code{"lm"} object.  
}
\item{ni}{
vector of the number of repetitions on the dependent variable. If the model
is poisson then \code{ni} is a vector of
\code{1}s.
}
\item{weights}{
weights from the final fit.
}
\item{iter}{
number of iterations used to compute the estimates.
}
\item{y}{
the dependent variable.
}
\item{contrasts}{
the same as the \code{contrasts} term of an
\code{"lm"} object. The object will also contain
other components related to the numerical fit that are not relevant for the
associated methods.  
}
}

\seealso{
\code{\link{glmRob}}.
}

\keyword{robust}
\keyword{regression}
\keyword{models}

