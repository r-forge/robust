rmodified <- function(object, ...)
  UseMethod("rmodified")


rmodified.default <- function(object, ...)
  residuals(object, ...) / sqrt(1.0 - hatvalues(object))


