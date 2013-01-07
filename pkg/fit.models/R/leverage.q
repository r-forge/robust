leverage <- function(object, ...)
  UseMethod("leverage")

leverage.default <- function(object, ...)
  rep(0.0, length(fitted(object)))

leverage.lm <- function(object, ...)
  lm.influence(object, do.coef = FALSE)$hat



