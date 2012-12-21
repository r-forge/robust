summary.lmfm <- function(object, correlation = FALSE, ...)
{
  object <- lapply(object, summary, correlation = correlation, ...)
  oldClass(object) <- "summary.lmfm"
  object
}


