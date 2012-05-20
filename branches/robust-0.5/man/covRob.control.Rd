\name{covRob.control}
\alias{covRob.control}

\title{
  Control Parameters for Robust Covariance Estimation
}

\description{
  This function is used to create a list of control parameters for the underlying robust estimator used in the \code{covRob} function. 
}

\usage{
  covRob.control(estim, ...)
}

\arguments{

  \item{estim}{a character vector of length one giving the name of the estimator to generate the control parameters for.}

  \item{\dots}{control parameters appropriate for the robust estimator specified in \code{estim} in the form \code{name = value} and separated by commas.  Omitted parameters receive their default values.}
}

\value{
  a list of control parameters appropriate for the robust estimator given in \code{estim}.  The value of \code{estim} occupies the first element of the list.}
  
\details{
  The control parameters are estimator specific.  Information on the control parameters (and their default values) can be found in the help files of each of the robust covariance estimators.
}

\seealso{
  This function is a utility function for \code{\link{covRob}}.<br>

  The underlying robust estimators are: \code{\link[rrcov]{CovSde}}, \code{\link[robustbase]{covMcd}} and \code{\link[rrcov]{CovOgk}}.  Power-users should consider calling these functions directly.
}

\examples{
  mcd.control <- covRob.control("mcd", quan = 0.75, ntrial = 1000)

  ds.control <- covRob.control("donostah", prob = 0.95)

  qc.control <- covRob.control("pairwiseqc")
}

%\keyword{librobust}
\keyword{utilities}
