\name{plot.glmfm}
\alias{plot.glmfm}

\title{Comparison Plots for Generalized Linear Models}

\description{
  Create a set plots useful for comparing the fitted generalized linear
  models in a glmfm object.  The plot options are
  \describe{
    \item{(2)}{Deviance Residuals vs. Fitted Values,}
    \item{(3)}{Response vs. Fitted Values,}
    \item{(4)}{Normal QQ Plot of Pearson Residuals,}
    \item{(5)}{QQ Plot of Deviance Residuals,}
    \item{(6)}{Standardized Deviance Residuals vs. Robust Distances,}
    \item{(7)}{Standardized Deviance Residuals vs. Index (Time),}
    \item{(8)}{Sqrt of abs(Deviance Residuals) vs. Fitted Values.}
    }
}

\usage{
\method{plot}{glmfm}(x, which.plots = "ask", ...)
}

\arguments{
  \item{x}{a \code{"glmfm"} object, i.e., typically produced from
    \code{\link{fit.models}(<glmRob>, etc)}.}

  \item{which.plots}{either "ask", "all", or an integer vector
    specifying which plots to draw.  In the latter case, use the plot
    numbers given in the description above (or in the "ask" menu).
  }

\item{...}{additional arguments are passed to the ploting subfunctions which are listed in the see also section.}
}

\section{Side Effects}{
The selected plots are drawn on a graphics device.
}

\details{
  This function is a wrapper for the \code{lmfm*} plot functions listed in
  the section \sQuote{See also}.  For finer control you may wish to call these
  functions directly.
}

\value{
\code{x} is invisibly returned.
}

\references{
  Atkinson, A. C. (1985).
  Plots, Transformations and Regression. New York: Oxford University Press.
}

\seealso{
  \code{\link{plot}}, \code{\link{fit.models}},
  \code{\link{lmfmResVsFittedPlot}},
  \code{\link{lmfmRespVsFittedPlot}},
  \code{\link{lmfmResQQPlot}},
  \code{\link{lmfmResVsRDPlot}}.
  \code{\link{lmfmResVsIdxPlot}},
  \code{\link{lmfmSqrtResVsFittedPlot}}.
}

\keyword{methods}
\keyword{hplot}


