\name{covRob}
\alias{covRob}

\title{
Robust Covariance/Correlation Matrix Estimation
}

\description{
Compute robust estimates of multivariate location and scatter.
}

\usage{
covRob(data, corr = FALSE, distance = TRUE, na.action = na.fail,
       estim = "auto", control = covRob.control(estim, ...), ...)
}

\arguments{
\item{data}{a numeric matrix or data frame containing the data.}

\item{corr}{a logical flag. If \code{corr = TRUE} then the estimated correlation matrix is computed.}

\item{distance}{a logical flag. If \code{distance = TRUE} the squared Mahalanobis distances are computed.}

\item{na.action}{a function to filter missing data. The default \code{na.fail} produces an error if missing values are present. An alternative is \code{na.omit} which deletes observations that contain one or more missing values.}

\item{estim}{a character string specifying the robust estimator to be used. The choices are: "mcd" for the Fast MCD algorithm of Rousseeuw and Van Driessen, "weighted" for the Reweighted MCD, "donostah" for the Donoho-Stahel projection based estimator, "M" for the constrained M estimator provided by Rocke, "pairwiseQC" for the orthogonalized quadrant correlation pairwise estimator, and "pairwiseGK" for the Orthogonalized Gnanadesikan-Kettenring pairwise estimator. The default "auto" selects from "donostah", "mcd", and "pairwiseQC" with the goal of producing a good estimate in a reasonable amount of time.}

\item{control}{a list of control parameters to be used in the numerical algorithms. See \code{covRob.control} for the possible control parameters and their default settings. This argument is ignored when \code{estim = "auto"}.}

\item{\dots}{control parameters may be passed directly when \code{estim != "auto"}.}
}

\details{
The \code{covRob} function selects a robust covariance estimator that is likely to provide a \emph{good} estimate in a reasonable amount of time. Presently this selection is based on the problem size. The Donoho-Stahel estimator is used if there are less than 1000 observations and less than 10 variables or less than 5000 observations and less than 5 variables. If there are less than 50000 observations and less than 20 variables then the MCD is used. For larger problems, the Orthogonalized Quadrant Correlation estimator is used.

The MCD and Reweighted-MCD estimates (\code{estim = "mcd"} and \code{estim = "weighted"} respectively) are computed using the \code{\link[robustbase]{covMcd}} function in the robustbase package. By default, \code{\link[robustbase]{covMcd}} returns the reweighted estimate; the actual MCD estimate is contained in the components of the output list prefixed with \code{raw}.

The M estimate (\code{estim = "M"}) is computed using the \code{\link[rrcov]{covMest}} function in the rrcov package. For historical reasons the Robust Library uses the MCD to compute the initial estimate.

The Donoho-Stahel (\code{estim = "donostah"}) estimator is computed using the \code{\link[rrcov]{CovSde}} function provided in the rrcov package.

The pairwise estimators (\code{estim = "pairwisegk"} and \code{estim = "pairwiseqc"}) are computed using the \code{\link[rrcov]{CovOgk}} function in the rrcov package.
}

\value{
an object of class "\code{covRob}" with components:

\item{call}{an image of the call that produced the object with all the arguments named.}

\item{cov}{a numeric matrix containing the final robust estimate of the covariance/correlation matrix.}

\item{center}{a numeric vector containing the final robust estimate of the location vector.}

\item{dist}{a numeric vector containing the squared Mahalanobis distances computed using robust estimates of covariance and location contained in \code{cov} and \code{center}. If \code{distance = FALSE} this element will me missing.}

\item{raw.cov}{a numeric matrix containing the initial robust estimate of the covariance/correlation matrix. If there is no initial robust estimate then this element is set to \code{NA}.}

\item{raw.center}{a numeric vector containing the initial robust estimate of the location vector. If there is no initial robust estimate then this element is set to \code{NA}.}

\item{raw.dist}{a numeric vector containing the squared Mahalanobis distances computed using the initial robust estimates of covariance and location contained in \code{raw.cov} and \code{raw.center}. If \code{distance = FALSE} or if there is no initial robust estimate then this element is set to \code{NA}.}

\item{corr}{a logical flag. If \code{corr = TRUE} then \code{cov} and \code{raw.cov} contain robust estimates of the correlation matrix of \code{data}.}

\item{estim}{a character string containing the name of the robust estimator.}

\item{control}{a list containing the control parameters used by the robust estimator.}
}

\references{
R. A. Maronna and V. J. Yohai (1995) The Behavior of the Stahel-Donoho Robust Multivariate Estimator. \emph{Journal of the American Statistical Association} \bold{90} (429), 330--341.

P. J. Rousseeuw and K. van Driessen (1999) A fast algorithm for the minimum covariance determinant estimator. \emph{Technometrics} \bold{41}, 212--223.

D. L. Woodruff and D. M. Rocke (1994) Computable robust estimation of multivariate location and shape on high dimension using compound estimators. \emph{Journal of the American Statistical Association}, \bold{89}, 888--896.

R. A. Maronna and R. H. Zamar (2002) Robust estimates of location and dispersion of high-dimensional datasets. \emph{Technometrics} \bold{44} (4), 307--317.
}

\note{
Version 0.3-8 of the Robust Library: all of the functions origianlly contributed by the S-Plus Robust Library have been replaced by dependencies on the robustbase and rrcov packages. Computed results may differ from earlier versions of the Robust Library. In particular, the MCD estimators are now adjusted by a small sample size correction factor. Additionally, a bug was fixed where the final MCD covariance estimate produced with \code{estim = "mcd"} was not rescaled for consistency.
}

\seealso{
  \code{\link[rrcov]{CovSde}},
  \code{\link[robustbase]{covMcd}},
  \code{\link[rrcov]{CovOgk}},
  \code{\link[rrcov]{covMest}},
  \code{\link{covRob.control}},
  \code{\link{covClassic}}.
}

\examples{
  data(stackloss)
  covRob(stackloss)
}

\keyword{multivariate}
\keyword{robust}
