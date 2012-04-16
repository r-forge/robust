plot.lmRob <- function(x, which.plots = ifelse(interactive(), "ask", "all"), ...)
{
	x.name <- deparse(substitute(x))
	model.list <- list(x$call)
	names(model.list) <- x.name
	fm <- list(x = x)
	names(fm) <- x.name
	attr(fm, "model.list") <- model.list
  oldClass(fm) <- "lmfm"

	plot(fm, which.plots = which.plots, ...)

	invisible(x)
}


