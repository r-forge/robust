\name{predict.glmRob}
\alias{predict.glmRob}

\title{Predict Method for Robust Generalized Linear Model Fits}

\description{
Obtains predictions and optionally estimates standard errors of those predictions from a fitted robust generalized linear model object.
}

\usage{
\method{predict}{glmRob}(object, newdata,
       type = c("link", "response", "terms"), se.fit = FALSE,
       terms = labels(object), dispersion = NULL, ...)
}

\arguments{
  \item{object}{a glmRob object.}

  \item{newdata}{optionally, a data frame in which to look for variables with which to predict. If omitted, the fitted linear predictors are used.}

  \item{type}{a character string specifying the type of prediction. The choices are "link" for predictions on the scale of the linear predictor, "response" for predctions on the scale of the response, and "terms" which returns a matrix giving the fitted values for each term in the model formula on the scale of the linear predictor.}

  \item{se.fit}{a logical value. If \code{TRUE} then standard errors for the predictions are computed.}

  \item{terms}{when \code{type = "terms"} all terms are returned. A character vector specifies which terms are to be returned.}

  \item{dispersion}{the dispersion of the generalized linear model fit to be assumed in computing the standard errors. If omitted, that returned by 'summary' applied to the object is used.}

  \item{\dots}{additional arguments required by the generic predict method.}
}

\value{
If \code{se.fit = FALSE}, a vector or matrix of predictions.  Otherwise a list with components:

  \item{fit}{Predictions}
  \item{se.fit}{Estimated standard errors}
}


\seealso{
\code{\link{glmRob}},
\code{\link{predict}}.
}

\examples{
data(breslow.dat)
bres.rob <- glmRob(sumY ~ Age10 + Base4 * Trt, family = poisson(), data = breslow.dat)
predict(bres.rob)
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


