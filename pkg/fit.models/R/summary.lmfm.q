summary.lmfm <- function(object, correlation = FALSE, ...)
{
  object <- lapply(object, summary, correlation = correlation, ...)

#  ## fix summary.lmrob objects ##
#  n.models <- length(object)
#  for(i in 1:n.models) {
#    if(is.null(object[[i]]$sigma)) object[[i]]$sigma <- object[[i]]$scale
#    if(length(object[[i]]$df) == 1) object[[i]]$df <- c(NA, object[[i]]$df, NA)
#  }
  ## end fix ##

  oldClass(object) <- "summary.lmfm"
  object
}


