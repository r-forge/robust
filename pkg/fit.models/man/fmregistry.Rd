\name{fmregistry}
\alias{fmclass.register}
\alias{fmclass.add.class}

\title{Register Comparable Functions}

\description{
The fit.models package maintains a list of comparable models called the fit.models registery.  These functions provide a method for adding new classes of models to the fit.models registery.
}

\usage{
fmclass.register(fmclass, classes, validation.function = NULL)

fmclass.add.class(fmclass, class, warn = TRUE)
}

\arguments{
  \item{fmclass}{a character string naming a fit.models class to be added to the \code{fit.models.registery}.}

  \item{classes}{a character vector naming one or more classes that can be compared by the methods defined for the fit.models class in \code{fmclass}.}

  \item{class}{a character string specifying a class compatible with the methods of \code{fmclass}.}

  \item{validation.function}{a function returning \code{TRUE} when the models are comparable.}

  \item{warn}{a logical value. If TRUE, a warning is printed if \code{class} is already registered in the \code{fit.models.registery}.}
}

\details{
See the package vignette.
}

\value{
a null value is invisibly returned.
}

\keyword{misc}


