\name{test.lmRob}
\alias{test.lmRob}

\title{Various Tests of Robust Regression Estimates }

\description{
Conducts test for bias of robust MM-estimates and Least Squares (LS) estimates against S-estimates, or permutation test of the slope estimate in a straight line fit. 
}

\usage{
test.lmRob(object, type = "bias", level = NULL, n.permute = 99)
}

\arguments{
\item{object}{an object of class \code{"lmRob"}.}

\item{type}{character string. Valid choices are \code{"bias"} for bias test, or "permutation" for permutation test.}

\item{level}{the level of the test for bias of MM-estimate.  By default, the \code{level} component of \code{object$robust.control} is used.}

\item{n.permute}{a positive integer value specifying the number of permutations to use.}
}

\value{
the p-value of the permutation test, or an object of class \code{"biasMM"} representing the bias test, in which case the following components ARE included: 

\item{mm}{a list describing the test of bias for final MM-estimates, with the following components: \code{stat}, the t-statistic; \code{pchi}, a chi-squared p-value; \code{qchi}, the quantile of the chi-squared distribution with degrees of freedom equal to \code{object\$rank} corresponding to the probability input in the level (\code{object$robust.control$level}).}

\item{ls}{a list describing the test of bias for LS-estimates, with the following components: \code{stat}, the t-statistic; \code{pchi}, a chi-squared p-value.}

\item{level}{the level of the test for bias of MM-estimate.}
}

\section{References}{
Yohai, V., Stahel, W. A., and Zamar, R. H. (1991). A procedure for robust estimation and inference in linear regression, in Stahel, W. A. and Weisberg, S. W., Eds., \emph{Directions in robust statistics and diagnostics, Part II}. Springer-Verlag. 
}

\keyword{robust}
\keyword{regression}


