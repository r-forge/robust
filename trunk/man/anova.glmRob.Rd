\name{anova.glmRob}
\alias{anova.glmRob}
\alias{anova.glmRoblist}

\title{ANOVA for Robust Generalized Linear Model Fits}

\description{
Compute an analysis of variance table for one or more robust generalized linear model fits.
}

\usage{
\method{anova}{glmRob}(object, ..., test = c("none", "Chisq", "F", "Cp"))
\method{anova}{glmRoblist}(object, ..., test = c("none", "Chisq", "F", "Cp")) 
}

\arguments{
\item{object}{a glmRob object.}

\item{\dots}{additional glmRob objects.}

\item{test}{a character string specifying the test statistic to be used. Can be one of "F", "Chisq", "Cp" or "none" for no test.}
}

\value{
an \code{anova} object.
}

\seealso{
\code{\link{glmRob}}, 
\code{\link{anova}}, 
\code{\link{anova.glmRoblist}}.
}

\examples{
data(breslow.dat)

bres.int <- glmRob(sumY ~ Age10 + Base4*Trt, family = poisson(), data = breslow.dat)
anova(bres.int)

bres.main <- glmRob(sumY ~ Age10 + Base4 + Trt, family = poisson(), data = breslow.dat)
anova(bres.main, bres.int)
}

\keyword{robust}
\keyword{regression}
\keyword{methods}


