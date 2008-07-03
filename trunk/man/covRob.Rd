\name{covRob}
\alias{covRob}

\title{
  Robust Covariance/Correlation Matrix Estimation
}

\description{
  Computes robust estimates of multivariate location and scatter.
}

\usage{
  covRob(data, corr = FALSE, distance = TRUE, na.action = na.fail, estim = "auto", control = covRob.control(estim, ...), ...)
}

  \arguments{

  \item{data}{a numeric matrix or data frame containing the data.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then the estimated correlation matrix is computed.}

  \item{distance}{a logical flag.  If \code{distance = TRUE} the Mahalanobis distances are computed.}

  \item{na.action}{a function to filter missing data.  The default \code{na.fail} produces an error if missing values are present.  An alternative is \code{na.omit} which deletes observations that contain one or more missing values.}

  \item{estim}{the robust estimator to be used.  The choices are: "mcd" for the Fast MCD algorithm of Rousseeuw and Van Driessen, "donostah" for the Donoho-Stahel projection based estimator, "M" for the constrained M estimator provided by Rocke, "pairwiseQC" for the orthogonalized quadrant correlation pairwise estimator, and "pairwiseGK" for the Orthogonalized Gnanadesikan-Kettenring pairwise estimator. The default "auto" selects from "donostah", "mcd", and "pairwiseQC" with the goal of producing a good estimate in a resonable amount of time.}

  \item{control}{a list of control parameters to be used in the numerical algorithms.  See \code{covRob.control} for the possible control parameters and their default settings.  This argument is ignored when \code{estim = "auto"}.}

  \item{\dots}{control parameters may be passed directly when \code{estim != "auto"}.}
}

\value{
  an object of class "\code{covRob}" with components:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{cov}{a numeric matrix containing the final robust estimate of the covariance/correlation matrix.}

  \item{center}{a numeric vector containing the final robust estimate of the location vector.}

  \item{dist}{a numeric vector containing the Mahalanobis distances computed using robust estimates of covariance and location contained in \code{cov} and \code{center}.  If \code{distance = FALSE} this element is set to \code{NA}.}

  \item{raw.cov}{a numeric matrix containing the initial robust estimate of the covariance/correlation matrix. If there is no initial robust estimate then this element is set to \code{NA}.}

  \item{raw.center}{a numeric vector containing the initial robust estimate of the location vector. If there is no initial robust estimate then this element is set to \code{NA}.}

  \item{raw.dist}{a numeric vector containing the Mahalanobis distances computed using the initial robust estimates of covariance and location contained in \code{raw.cov} and \code{raw.center}. If \code{distance = FALSE} or if there is no initial robust estimate then this element is set to \code{NA}.}

  \item{corr}{a logical flag.  If \code{corr = TRUE} then \code{cov} and \code{raw.cov} contain robust estimates of the correlation matrix of \code{data}.}

  \item{estim}{a character string containing the name of the robust estimator.}

  \item{control}{a list containing the control parameters used by the robust estimator.}
}

\details{

}

\seealso{
  \code{\link{covRob.control}},
  \code{\link{cov}},
  \code{\link[robustbase]{covMcd}},
  \code{\link{donostah}},
  \code{\link{fastcov}}.
}

\examples{
  data(stackloss)
  covRob(stackloss, estim = "mcd", quan = .75, ntrial = 1000)
}

\keyword{multivariate}
\keyword{robust}
