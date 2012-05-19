\name{step.lmRob}
\alias{step.lmRob}

\title{Build a Model in a Stepwise Fashion}

\description{
Performs stepwise model selection on a robustly fitted linear model.  Presently only the backward stepwise procedure is implemented.
}

\usage{
step.lmRob(object, scope, scale,
           direction = c("both", "backward", "forward"),
           trace = TRUE, keep = NULL, steps = 1000, fast = FALSE, \dots)
}

\arguments{
\item{object}{an \code{\link{lmRob}} object.}

\item{scope}{either a formula or a list with elements \code{lower} and \code{upper} each of which is a formula.  The terms in the right-hand-side of \code{lower} are always included in the model and the additional terms in the right-hand-side of \code{upper} are the candidates for inclusion/exclusion from the model.  If a single formula is given, it is taken to be \code{upper} and \code{lower} is set to the empty model.  The \code{.} operator is interpreted in the context of the formula in \code{object}.}

\item{scale}{a single numeric value containing a residual scale estimate.  If missing, the scale estimate in \code{object} is used.}

\item{direction}{a character value specifying the mode of stepwise search.  The possibilities are "both", "backward", and "forward", with a default of "backward".  Presently only "backward" stepwise searches are implemented.}

\item{trace}{a logical value.  If \code{TRUE}, information is printed during stepwise search.}

\item{keep}{a filter function whose input is a fitted model object and the associated AIC statistic, and whose output is arbitrary.  Typically keep will select a subset of the components of the object and return them.  The default is not to keep anything.}

\item{steps}{an integer value specifying the the maximum number of steps to be considered.  The default is 1000 (essentially as many as required).  It is typically used to stop the process early.}

\item{fast}{a logical value.  If \code{TRUE} the robust initial estimate (used when fitting each of the reduced models) is replaced by a weighted least squares estimate using the robust weights computed for the current fit.  Note: the fast algorithm does not work in this version of the Robust Library.}

\item{\dots}{additional arguments required by the generic step function.}
}

\details{
Presently only backward stepwise selection is supported.  During each step the Robust Final Prediction Error (as computed by the function \code{\link{lmRob.RFPE}}) is calculated for the current model and for each sub-model achievable by deleting a single term.  The function then either steps to the sub-model with the lowest Robust Final Prediction Error or, if the current model has the lowest Robust Final Prediction Error, terminates.  The scale estimate from \code{object} is used to compute the Robust Final Prediction Error throughout the procedure unless the \code{scale} argument is provided in which case the user specified value is used.
}

\value{
the model with the lowest Robust Final Prediction Error encountered during the stepwise procedure is returned.  Additionally, an \code{anova} element corresponding to the steps taken in the search is appended to the returned object.  If a \code{keep} function was provided then the kept values can be found in the \code{keep} element of the returned object.
}

\seealso{
  \code{\link{lmRob}},
  \code{\link{lmRob.RFPE}},
  \code{\link{drop1.lmRob}}.
}

\examples{
data(stack.dat)
stack.rob <- lmRob(Loss ~ ., data = stack.dat)

## The default behavior is to try dropping all terms ##
step.lmRob(stack.rob)

## Keep Water.Temp in the model ##
my.scope <- list(lower = . ~ Water.Temp, upper = . ~ .)
step.lmRob(stack.rob, scope = my.scope)
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


