\name{fm.registry}
\alias{fm.register.vclass}
\alias{fm.register.class.in.vclass}

\title{Register Comparable Functions}

\description{
The fit.models package maintains a list of comparable models caled the \code{fit.models.registery}.  These functions provide a method for adding new classes of models to the \code{fit.models.registery}.
}

\usage{
fm.register.vclass(vclass, classes, validation.function = NULL, attributes.function = NULL)

fm.register.class.in.vclass(vclass, class)
}

\arguments{
  \item{vclass}{a character string naming a virtual class to be added to the \code{fit.models.registery}.}

  \item{classes}{a character vector naming one or more classes that can be compared by the methods defined for the virtual class in \code{vclass}.}

  \item{class}{a character string specifying a class compatible with the methods of \code{vclass}.}

  \item{validation.function}{a function returning \code{TRUE} when the models are comparable.}

  \item{attributes.function}{a function that adds attributes to the object created by fit.models.}
}

\details{
See the package vignette.
}

\value{
a null value is invisibly returned.
}

\keyword{misc}


