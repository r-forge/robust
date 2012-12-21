\name{fit.models}
\alias{fit.models}

\title{Fit dot Models}

\description{
  Combines comparable models into single object.  Generic methods produce side-by-side comparisons of the parameter estimates and disagnostic plots.
}

\usage{
  fit.models(model.list, \dots)
}

\arguments{
  \item{model.list}{either a list of calls or a list of function names.  This argument is not required when combining one or more fitted models into a fit.models object.}

  \item{\dots}{additional arguments to be passed to the functions in \code{model.list}.}
}

\value{
  The returned object is a list containing the fitted models.  The class of the retuned object depends on the classes of the model objects it contains.
}

\details{
If \code{model.list} is a list of function names then \code{fit.models} creates a call to each of the functions in \code{model.list} by combining the each function name with the remaining arguments passed to \code{fit.models}.  The list of calls is then evalued as described above.

If \code{model.list} is a fitted model object then \code{fit.models} assumes that each of its argument is a fitted model object.  The arguments are put into a list, the appropriate vclass is set, and the list is returned.

To be comparable the specified models must all belong to one of the supported classes in fit.models.
}

\examples{
  data(stackloss)

  lm.full <- lm(stack.loss ~ ., data = stackloss)
  lm.no <- lm(stack.loss ~ ., data = stackloss, subset = 5:20)

  stack.fm <- fit.models("All Observations" = lm.full, "No Outliers" = lm.no)
  stack.fm
  summary(stack.fm)
  plot(stack.fm)
}

\keyword{models}
