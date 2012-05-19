print.fit.models <- function(x, models = 1:length(x), ...)
{
	orig.class <- class(x)
	new.class <- attr(x, "virtual.class")
	if(!exists(paste("print", new.class, sep = ".")))
		stop(paste("No print method defined for virtual class", new.class,
			".", sep = ""))
	model.list <- attr(x, "model.list")[models]
	x <- x[models]
	attr(x, "model.list") <- model.list
	oldClass(x) <- new.class
	print(x, ...)
	oldClass(x) <- orig.class
	invisible(x)
}


coef.fit.models <- function(object, ...)
{
	if(!exists(paste("coef", attr(object, "virtual.class"), sep = ".")))
		stop(paste("No coef method defined for virtual class ",
			attr(object, "virtual.class"), ".", sep = ""))
	oldClass(object) <- attr(object, "virtual.class")
	coef(object, ...)
}


residuals.fit.models <- function(object, ...)
{
	if(!exists(paste("residuals", attr(object, "virtual.class"), sep = ".")))
		stop(paste("No residuals method defined for virtual class ",
			attr(object, "virtual.class"), ".", sep = ""))
	oldClass(object) <- attr(object, "virtual.class")
	residuals(object, ...)
}


summary.fit.models <- function(object, models = 1:length(object), ...)
{
	new.class <- attr(object, "virtual.class")
	if(!exists(paste("summary", new.class, sep = ".")))
		stop(paste("No summary method defined for virtual class", new.class,
			".", sep = ""))
	model.list <- attr(object, "model.list")[models]
	object <- object[models]
	attr(object, "model.list") <- model.list
	oldClass(object) <- new.class
	summary(object, ...)
}


plot.fit.models <- function(x, ...)
{
	if(!exists(paste("plot", attr(x, "virtual.class"), sep = ".")))
		stop(paste("No plot method defined for virtual class ",
			attr(x, "virtual.class"), ".", sep = ""))
	orig.class <- class(x)
	oldClass(x) <- attr(x, "virtual.class")
	plot(x, ...)
	oldClass(x) <- orig.class
	invisible(x)
}


dummy.coef.fit.models <- function(object, ...)
{
	if(!exists(paste("dummy.coef", attr(object, "virtual.class"), sep = ".")))
		stop(paste("No dummy.coef method defined for virtual class ",
			attr(object, "virtual.class"), ".", sep = ""))
	oldClass(object) <- attr(object, "virtual.class")
	dummy.coef(object, ...)
}


