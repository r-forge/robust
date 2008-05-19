summary.glmfm <- function(object, correlation = FALSE, ...)
{
	model.list <- attr(object, "model.list")
	object <- lapply(object, summary, correlation = correlation, ...)
	attr(object, "model.list") <- model.list
	oldClass(object) <- "summary.glmfm"
	object
}


