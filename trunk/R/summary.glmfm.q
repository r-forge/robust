summary.glmfm <- function(object, correlation = FALSE, ...)
{
  object <- lapply(object, summary, correlation = correlation, ...)
  oldClass(object) <- "summary.glmfm"
  object
}


