\name{breslow.dat}
\alias{breslow.dat}

\docType{data}

\title{Breslow Data}

\description{
Patients suffering from simple or complex partial seizures were randomized to receive either the antiepileptic drug progabide or a placebo.  At each of four successive postrandomization clinic visits, the number of seizures occuring over the previous two weeks was reported.
}

\usage{data(breslow.dat)}

\format{
  A data frame with 59 observations on the following 12 variables.
  \describe{
    \item{\code{ID}}{an integer value specifying the patient identification number.}
    \item{\code{Y1}}{an integer value, the number of seizures during the first two week period.}
    \item{\code{Y2}}{an integer value, the number of seizures during the second two week period.}
    \item{\code{Y3}}{an integer value, the number of seizures during the third two week period.}
    \item{\code{Y4}}{an integer value, the number of seizures during the fourth two week period.}
    \item{\code{Base}}{an integer value giving the eight-week baseline seizure count.}
    \item{\code{Age}}{an integer value giving the age of the parient in years.}
    \item{\code{Trt}}{the treatment: a factor with levels \code{placebo} and \code{progabide}.}
    \item{\code{Ysum}}{an integer value, the sum of \code{Y1}, \code{Y2}, \code{Y3} and \code{Y4}.}
    \item{\code{sumY}}{an integer value, the sum of \code{Y1}, \code{Y2}, \code{Y3} and \code{Y4}.}
    \item{\code{Age10}}{a numeric value, \code{Age} divided by 10.}
    \item{\code{Base4}}{a numeric value, \code{Base} divided by 4.}
  }
}

\references{
Breslow, N. E., and Clayton, D. G. (1993), "Approximate Inference in Generalized Linear Mixed Models," Journal of the American Statistical Association, Vol. 88, No. 421, pp. 9-25.

Thrall, P. F., and Vail, S. C. (1990), "Some Covariance Models for Longitudinal Count Data With Overdispersion," Biometrics, Vol. 46, pp. 657-671.
}

\examples{
data(breslow.dat)
}

\keyword{datasets}
