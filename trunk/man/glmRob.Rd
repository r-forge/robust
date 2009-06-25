\name{glmRob}
\alias{glmRob}

\title{Fit a Robust Generalized Linear Model}

\description{
Produces an object of class \code{glmRob} which is a Robust Generalized Linear Model fit.}

\usage{
glmRob(formula, family = binomial(), data, weights, subset,
       na.action, method = "cubif",
       model = TRUE, x = FALSE, y = TRUE,
       control = glmRob.control, contrasts = NULL, \dots)
}

\arguments{
\item{formula}{a formula expression as for other regression models, of the form response ~ predictors. See the documentation of \code{lm} and \code{formula} for details.}

\item{family}{a family object - only \code{binomial} and \code{poisson} are implemented. See the documentation of \code{glm} for details.}

\item{data}{an optional data frame in which to interpret the variables occuring in the formula.}

\item{weights}{an optional vector of weights to be used in the fitting process.  Should be \code{NULL} or a numeric vector.}

\item{subset}{an expression specifying the subset of the data to which the model is fit. This can be a logical vector (which is replicated to have length equal to the number of observations), a numeric vector indicating which observations are included, or a character vector of the row names to be included. By default all observations are used.}

\item{na.action}{a function to filter missing data. This is applied to the \code{model.frame} after any \code{subset} argument has been used. The default (\code{na.fail}) is to create an error if any missing values are found. A possible alternative is \code{na.omit} which omits the rows that contain one or more missing values.}

\item{method}{a character vector indicating the fitting method. The choices are \code{method = "cubif"} for the conditionally unbiased bounded influence estimator, \code{method = "mallows"} for Mallow's leverage downweighting estimator, and \code{method = "misclass"} for a consistent estimate based on the misclassification model. The Mallows' and misclassification estimators are only defined for logistic regression models with Bernoulli response.}

\item{model}{a logical flag. If \code{TRUE} then the model frame is returned.}

\item{x}{a logical flag. If \code{TRUE} then the model matrix is returned.}

\item{y}{a logical flag. If \code{TRUE} then the response variable is returned.}

\item{contrasts}{a list of contrasts to be used for some or all of the factors appearing as variables in the model formula. The names of the list should be the names of the corresponding variables, and the elements should either be contrast-type matrices (matrices with as many rows as levels of the factor and with columns linearly independent of each other and of a column of one's), or else they should be functions that compute such contrast matrices.}

\item{control}{a list of iteration and algorithmic constants to control the conditionally unbiased bounded influence robust fit. See \code{glmRob.cubif.control} for their names and default values. These can also be set as arguments of \code{glmRob} itself.}

\item{\dots}{control arguments maybe specified directly.}

}

\value{a list with class \code{glmRob} containing the robust generalized linear model fit. See \code{\link{glmRob.object}} for details.}

\section{References}{
Copas, J. B. (1988). Binary Regression Models for Contaminated Data. JRSS 50, 225-265.

Kunsch, L., Stefanski L. and Carroll, R. (1989). Conditionally Unbiased Bounded-Influence Estimation in General Regression Models, with Applications to Generalized Linear Models. JASA 50, 460-466.

Carroll, R. J. and Pederson, S. (1993). On Robustness in the Logistic Regression Model. JRSS 55, 693-706.

Marazzi, A. (1993). Algorithms, routines and S functions for robust statistics. Wadsworth & Brooks/Cole, Pacific Grove, CA.
}

\seealso{
\code{\link{glmRob.control}},
\code{\link{glmRob.object}},
\code{\link{glmRob.cubif.control}},
\code{\link{glmRob.mallows.control}},
\code{\link{glmRob.misclass.control}},
\code{\link{glm}}.
}

\examples{
data(breslow.dat)

glmRob(sumY ~ Age10 + Base4*Trt, family = poisson(),
       data = breslow.dat, method = "cubif")
}

\keyword{robust}
\keyword{regression}
\keyword{models}
