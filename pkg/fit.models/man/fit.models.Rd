\name{fit.models}
\alias{fit.models}

\title{Fit dot Models}

\description{
Fit a statistical model using different estimators (e.g., robust and least-squares) and/or combine fitted models into a single object.  Generic methods then produce side-by-side comparisons of the parameter estimates and diagnostic plots.
}

\usage{
  fit.models(model.list, \dots)
}

\arguments{
  \item{model.list}{a list or a character vector containing names of modeling functions.  Only required when \code{fit.models} is being used to fit models (rather than combine already fitted models into a \code{fit.models} object).}

  \item{\dots}{see details.}
}

\value{
  The returned object is a list containing the fitted models.  The class of the retuned object depends on the classes of the model objects it contains.
}

\details{
There are two distinct ways the fit.models function can be used.

The first is to fit the same model using different estimators.  In this case, \code{model.list} should be a character vector or a list where each element is the name of a modeling function and the remaining arguments (in \dots) are the common arguments to the functions in \code{model.list}.  For example, the following command fits robust and least-squares linear models to Brownlee's Stack Loss Plant Data.
\preformatted{    fit.models(c("rlm", "lm"), stack.loss ~ ., data = stackloss)}
The resulting fit.models object is then a list with the output of
\preformatted{    rlm(stack.loss ~ ., data = stackloss)}
in the first element and
\preformatted{    lm(stack.loss ~ ., data = stackloss)}
in the second.  The class attribute of the returned list is set (in this case) to \code{"lmfm"} which is the fit.models class (fmclass) for comparing linear-model-like fits.

The second use of fit.models is to combine already fitted model objects.  In this case fit.models combines its arguments into a fit.models object (a list where element i is occupied by argument i) and sets the class attribute to the most appropriate fit.models class.
}

\seealso{
\code{\link{fmclass.add.class}} for adding a class to an existing fit.models class and \code{\link{fmclass.register}} to create a new fit.models class.
}

\examples{
  data(stackloss)

  # First, use fit.models to fit robust and least-squares linear
  # regression models to the Brownlee's Stack Loss Plant Data.

  # Step 1: rlm (robust linear model) is in the MASS package.
  require(MASS)

  # Step 2: tell fit.models rlm can be compared to lm
  fmclass.add.class("lmfm", "rlm")

  fm1 <- fit.models(c("rlm", "lm"), stack.loss ~ ., data = stackloss)

  summary(fm1) #rlm does not provide p-values or Multiple R-squared
  plot(fm1)


  # Second, use fit.models to combine already fitted models into a
  # fit.models object.

  lm.complete <- lm(stack.loss ~ ., data = stackloss)
  lm.clean <- lm(stack.loss ~ ., data = stackloss, subset = 5:20)

  fm2 <- fit.models(lm.clean, lm.complete)

  summary(fm2)
  plot(fm2)


  # Name the models in the fit.models object.
  
  fm3 <- fit.models(c(Robust = "rlm", "Least-Squares" = "lm"),
                    stack.loss ~ ., data = stackloss)

  fm4 <- fit.models(Clean = lm.clean, Complete = lm.complete)
}

\keyword{models}
