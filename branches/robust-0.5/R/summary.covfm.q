summary.covfm <- function(object, ...)
{
  object <- lapply(object, summary, ...)
  oldClass(object) <- "summary.covfm"
  object
}


