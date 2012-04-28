summary.asmfm <- function(object, ...)
{
  object <- lapply(object, summary, ...)
  oldClass(object) <- "summary.asmfm"
  object
}


