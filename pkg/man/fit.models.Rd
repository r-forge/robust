\name{fit.models}
\alias{fit.models}

\title{
  Model Comparison
}

\description{
  Combines comparable models into single object.  Generic methods produce side-by-side comparisons of the estimated model parameters and disagnostic plots.
}

\usage{
  fit.models(model.list, formula = NULL, ...)
}

\arguments{
  \item{model.list}{either a list of calls or a list of function names.  This argument is not required when combining one or more fitted models into a fit.models object.}

  \item{formula}{when \code{model.list} is a list of function names the second argument is often a formula.  This saves you the trouble of having to name it explicitly when calling fit.models.}

  \item{\dots }{additional arguments to be passed to the functions in \code{model.list}.}
}

\value{
  The returned object is a list containing the fitted models.  The class of the retuned object depends on the classes of the model objects it contains.  Returned objects containing class "cov" and "covRob" objects have class "covfm".
}

\details{
If \code{model.list} is a list of calls then \code{fit.models} replaces each element by its evaluated call and sets the appripriate class.

If \code{model.list} is a list of function names then \code{fit.models} creates a call to each of the functions in \code{model.list} by combining the respective function name with the remaining arguments passed to \code{fit.models}.  The list of calls is then evalued as described above.

If \code{model.list} is a fitted model object then \code{fit.models} assumes that each of its argument is a fitted model object.  The arguments are put into a list, the appropriate class is set, and the list is returned.
To be comparable the specified models must all belong to one of the supported classes in fit.models.  The supported classes of models in this release of the Robust Library are (1) Covariance/Correlation Models \{"cov", "covRob"\}.  The subfunction \code{get.fit.models.database} provides \code{fit.models} with the classes of comparable models.
}

\seealso{
  \code{\link{get.fit.models.database}}.
}

\examples{
  data(woodmod.dat)
  woodmod.fm <- fit.models(list(Robust = "covRob", Classical = "cov"), data = woodmod.dat)

  #This way doesn't work in R yet
  #woodmod.cls <- cov(woodmod.dat)
  #woodmod.rob <- covRob(woodmod.dat)
  #woodmod.fm <- fit.models(Robust = woodmod.rob, Classical = woodmod.cls)
}

\keyword{models}

