\name{plot.glmfm}
\alias{plot.glmfm}

\title{Comparison Diagnostic Plots for Generalized Linear Models}

\description{
Produces a set of comparison diagnostic plots.  The plot options are
\describe{
  \item{(2)}{Deviance Residuals vs. Predicted Values,}
  \item{(3)}{Response vs. Fitted Values,}
  \item{(4)}{Normal QQ Plot of Modified Pearson Residuals,}
  \item{(5)}{Normal QQ Plot of Modified Deviance Residuals,}
  \item{(6)}{Modified Pearson Residuals vs. Leverage,}
  \item{(7)}{Scale-Location.}
}
}

\usage{
\method{plot}{glmfm}(x, which.plots = c(2, 5, 7, 6), \dots)
}

\arguments{
  \item{x}{a \code{glmfm} object.}

\item{which.plots}{either \code{"ask"}, \code{"all"}, or a vector of integer values specifying which plots to draw.  In the latter case, use the plot numbers given in the description above (or in the "ask" menu).  Any other values will be silently ignored.}

\item{\dots}{other parameters to be passed through to plotting functions.}
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\value{
\code{x} is invisibly returned.
}

\seealso{
\code{\link{qqPlot.lmfm}} for (4) and (5) and \code{\link{scatterPlot.lmfm}} for the others.
}

\examples{
# From ?glm:
# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)

clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))

lot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
lot2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)

fm <- fit.models(lot1, lot2)
plot(fm)
}

\keyword{hplot}
\keyword{methods}


