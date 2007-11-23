summary.covfm <- function(object, ...)
{
	model.list <- attr(object, "model.list")
	object <- lapply(object, summary, ...)
	attr(object, "model.list") <- model.list
	oldClass(object) <- "summary.covfm"
	object
}


