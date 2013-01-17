\name{plot.lmfm}
\alias{plot.lmfm}

\title{Comparison Diagnostic Plots for Linear Regression Models}

\description{
Produces a set of comparison diagnostic plots.  The plot options are 

\describe{
  \item{(2)}{Normal QQ Plot of Modified Residuals,}
  \item{(3)}{Kernel Density Estimate of Modified Residuals,}
  \item{(4)}{Modified Residuals vs. sqrt(Leverage),}
  \item{(5)}{Modified Residuals vs. Fitted Values,}
  \item{(6)}{Scale-Location,}
  \item{(7)}{Response vs. Fitted Values,}
  \item{(8)}{Modified Residuals vs. Index (Time),}
  \item{(9)}{Overlaid Normal QQ Plot of Modified Residuals,}
  \item{(10)}{Overlaid Kernel Density Estimate of Modified Residuals,}
  \item{(11)}{Scatter Plot with Overlaid Fits (for simple linear regression models).}
}
}

\usage{
\method{plot}{lmfm}(x, which.plots = c(5, 2, 6, 4),  \dots)
}

\arguments{
\item{x}{an \code{lmfm} object.}

\item{which.plots}{either \code{"ask"}, \code{"all"}, or a vector of integer values specifying which plots to draw.  In the latter case, use the plot numbers given in the description above (or in the "ask" menu).  Any other values will be silently ignored.}

\item{\dots}{other parameters to be passed through to plotting functions.}
}

\value{
\code{x} is invisibly returned.
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\details{
The \emph{modified residuals} are defined to be

\deqn{r_{i} = \frac{e_{i}}{\sqrt{1 - h_{i}}}}

where \eqn{h_{i} = H_{ii}} is the \eqn{i^{th}} diagonal element of the hat matrix.  The modified residuals are identically distributed with variance \eqn{\sigma^{2}}.  The modified residuals are used instead of the standardized residuals (which are identically distributed with variance 1) so that the comparison plots emphasize differences in the variance estimates.
}

\section{References}{
Atkinson, A. C. 1985. \emph{Plots, transformations, and regression: an introduction to graphical methods of diagnostic regression analysis}. Oxford: Clarendon Press.
}

\seealso{
See \code{\link{qqPlot.lmfm}} for (2), \code{\link{kernDenPlot.lmfm}} for (3), \code{\link{indexPlot.lmfm}} for (8), \code{\link{overlaidQQPlot.lmfm}} for (9), \code{\link{overlaidKernDenPlot.lmfm}} for (10), \code{\link{simpleRegPlot.lmfm}} for (11), and \code{\link{scatterPlot.lmfm}} for the others.  See \code{\link{rmodified}} for modified residuals.
}

\examples{
data(stackloss)
stack.lm <- lm(stack.loss ~ ., data = stackloss)
stack.clean <- lm(stack.loss ~ ., data = stackloss, subset = 5:20)
fm <- fit.models(stack.clean, stack.lm)
plot(fm)
}

\keyword{hplot}
\keyword{methods}


