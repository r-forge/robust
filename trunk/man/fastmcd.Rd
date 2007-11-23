\name{fastmcd}
\alias{fastmcd}

\title{
  Fast MCD Estimation
}

\description{
Returns a list of class mcd containing estimates of the robust multivariate location, the robust covariance matrix, and optionally the robust correlation matrix.  Specifically, the fastmcd function first returns the raw minimum covariance determinant (MCD) estimator of Rousseeuw (1984, 1985).  Then the MCD estimate is used to assign weights to the objects, and also weighted estimates of location and covariance are returned.
}

\usage{
fastmcd(x, cor = FALSE, print.it = TRUE, quan = floor((n + p + 1)/2), ntrial = 500)
}

\arguments{
  \item{x}{a vector, matrix, or data frame.  Columns represent variables, rows represent observations.  Missing values (NAs) and Infinite values (Infs) are allowed.  Observations (rows) with missing or infinite values are automatically excluded from the computations.}

  \item{cor}{a logical flag.  If \code{cor = TRUE} then the estimated correlation matrix will be returned as well.}

  \item{print.it}{a logical flag.  If \code{print.it = TRUE} information about the method will be printed.}

  \item{quan}{an integer value giving the number of observations whose covariance determinant will be minimized.  The default \code{quan} is \code{floor((n+p+1)/2)}, where \code{n} is the number of observations and \code{p} is the number of variables.  Any \code{quan} between the default and \code{n} may be specified.}

  \item{ntrial}{the number of random trial subsamples that are drawn for large datasets.  The default is 500.}
}

\value{
  an object of class "\code{mcd}" with components:

  \item{call}{an image of the call that produced the object with all the arguments named.}

  \item{method}{a character string that contains information about the method and about singular subsamples (if any).}

  \item{quan}{the number of observations that have determined the minimum covariance determinant estimator.  The default is \code{floor((n+p+1)/2)}, where \code{n} is the number of observations and \code{p} the number of variables.}

  \item{mcd.wt}{weights based on the estimated covariance matrix and the estimated location of the data.}

  \item{X}{the input data.}

  \item{raw.cov}{the raw MCD covariance matrix.}

  \item{raw.center}{the raw MCD location of the data.}

  \item{raw.objective}{the determinant of the raw MCD covariance matrix.}

  \item{cov}{the robust covariance matrix obtained by reweighting.  (If the raw MCD is singular, it is given here.)}

  \item{cor}{the estimated correlation matrix for the data.  This is only returned if \code{cor = TRUE}.}

  \item{center}{the robust location estimate of the data, obtained by reweighting.  (If the raw MCD is singular, its center is given here.)}

  \item{n.obs}{the number of data observations (after any missing values have been removed).}
}

\section{Side Effects}{
  If \code{print.it = TRUE} a message is printed.
}

\details{
Let \code{n} be the number of observations and \code{p} be the number of variables.  The minimum covariance determinant estimate is given by the subset of \code{quan} observations of which the determinant of their covariance matrix is minimal.  The MCD location estimate is then the mean of those \code{quan} points, and the MCD scatter estimate is their covariance matrix.  The default value of \code{quan} is \code{floor((n+p+1)/2)}, but the user may choose a larger number.  For multivariate data sets, it takes too much time to find the exact estimate, so an approximation is computed.  A full description of the present algorithm can be found in Rousseeuw and Van Driessen (1997).  Major advantages of this algorithm are its precision and the fact that it can deal with very large \code{n}.


Although the raw minimum covariance determinant estimate has a high breakdown value, its statistical efficiency is low.  A better finite-sample efficiency can be attained while retaining the high breakdown value by computing a weighted mean and covariance estimate, with weights based on the MCD estimate.  By default, fastmcd returns both the raw MCD estimate and the weighted estimate.


Multivariate outliers can be found by means of the robust distances, as described in Rousseeuw and Leroy (1987) and in Rousseeuw and Van Zomeren (1990).  These distances can be calculated by the function \code{mahalanobis}, and plotted by applying plot.mcd on a "mcd" object.  It is suggested that the number of observations be at least five times the number of variables.  When there are fewer observations than this, there is not enough information to determine whether outliers exist.


An important advantage of the present algorithm is that it allows for exact fit situations, where more than quan observations lie on a hyperplane.  Then the program still yields the MCD location and scatter matrix, the latter being singular (as it should be), as well as the equation of the hyperplane.


If the classical covariance matrix of the data is already singular, all observations lie on a hyperplane.  Then fastmcd will give a message and the equation of the hyperplane.  The MCD estimates are then equal to the classical estimates. In this case, you will need to modify your data before applying fastmcd, perhaps by using princomp and deleting columns with zero variance.


For univariate data sets, the exact algorithm location.lts is used. See the location.lts help file for more information.
}

\section{Background}{
The minimum covariance determinant estimator (Rousseeuw, 1985) has a breakdown value of roughly \code{(n-quan)/n}, which is about \code{50\%} for the default quan. That is, the estimate cannot be made arbitrarily bad without changing about half of the data.  A covariance matrix is considered to be arbitrarily bad if some eigenvalue goes to infinity or to zero (singular matrix).  This is analogous to a univariate scale estimate, which breaks down if the estimate is going either to infinity or to zero.
}

\section{References}{

Rousseeuw, P. J. and Van Driessen, K. (1999).  A fast algorithm for the minimum covariance determinant estimator.  \emph{Technometrics}, 41, 212-223.

Rousseeuw, P. J. (1984).  Least median of squares regression.  \emph{Journal of the American Statistical Association}, 79, 871-881.

Rousseeuw, P. J. (1985).  Multivariate estimation with high breakdown point.  In Mathematical Statistics and Applications. W. Grossmann, G. Pflug, I. Vincze and W. Wertz, eds.  Reidel: Dordrecht, 283-297.

Rousseeuw, P. J. and Leroy, A. M. (1987).  Robust Regression and Outlier Detection.  Wiley-Interscience, New York. [Chapter 7]

Rousseeuw, P. J. and van Zomeren, B. C. (1990).  Unmasking multivariate outliers and leverage points.  \emph{Journal of the American Statistical Association}, 85, 633-639.
}

\seealso{
  \code{\link{covRob}}.
}

\examples{
  data(stack.dat)
  fastmcd(stack.dat)
}

%\keyword{librobust}
\keyword{multivariate}
\keyword{robust}
