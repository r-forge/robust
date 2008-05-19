fit.models <- function(model.list, formula = NULL, ...)
{

	fmdb <- get.fit.models.database()

	##
	##	model.list should be either a list of function names, (in
	##	which case you must also provide additional arguments in 
	##	the ... field), or a list of calls.  If there is an entry
	##	in .fit.models.database for the supplied models then
	##	fit.models will return a "fit.models" class object
	##	containing the models.
	##
	##	Additionally, if fit.models is called on a single object,
	##	that object will be coerced to a fit.models object if there
	##	is an entry for it in .fit.models.database.
	##

	the.call <- match.call()

	if(class(model.list) == "list") {

		n.models <- length(model.list)

		if(is.character(model.list[[1]])) {

			fun.call <- the.call
			fun.call$model.list <- NULL

	##	construct model.list
	##	it will be evaluated in the next if(...)

			ans <- list()
			for(i in 1:n.models) {
				temp <- model.list[[i]]
				model.list[[i]] <- fun.call
				model.list[[i]][[1]] <- as.name(temp)
			}
		}

	##	a list of calls to be evaulated and stored in a fit.models
	##	object

		if(is.call(model.list[[1]])) {
			models <- sapply(model.list, function(u) as.character(u[[1]]))
			db.index <- -1

			for(i in 1:length(fmdb)) {
				temp <- match(models, fmdb[[i]]$classes, nomatch = 0)
				if(prod(temp) != 0) {
					db.index <- i
					break
				}
			}

			if(db.index == -1)
				stop("Specified models are not comparable.")
	
			if(is.null(names(model.list)))
				list.names <- paste("Model", 1:n.models, sep = ".")
			else
				list.names <- names(model.list)

	##	check the validity of model comparison

			if(!is.null(fmdb[[db.index]]$validate.function)) {
				tt <- call(fmdb[[db.index]]$validate.function, x = model.list)
				tt <- eval(tt, sys.parent())
			}

			ans <- list()
			for(i in 1:n.models) {
				ans[[i]] <- try(eval(model.list[[i]], sys.parent()))
				model.list[[i]] <- ans[[i]]$call
			}
	
			names(ans) <- list.names
			attr(ans, "model.list") <- model.list
			oldClass(ans) <- fmdb[[db.index]]$object.class
		}

	##	a list of already fitted models to be combined into a
	##	fit.models object

		else {

			models <- as.character(lapply(model.list, function(u) u$call[[1]]))
			db.index <- -1

			for(i in 1:length(fmdb)) {
				temp <- match(models, fmdb[[i]]$classes, nomatch = 0)
				if(prod(temp) != 0) {
					db.index <- i
					break
				}
			}

			if(db.index == -1)
				stop("Specified models are not comparable.")
	
			if(is.null(names(model.list)))
				names(model.list) <- paste("Model", 1:n.models, sep = ".")
			
			ans <- model.list
			model.list <- lapply(ans, function(u) u$call)
			attr(ans, "model.list") <- model.list
			oldClass(ans) <- fmdb[[db.index]]$object.class

	##	validate model comparison

			if(!is.null(fmdb[[db.index]]$validate.function)) {
				tt <- call(fmdb[[db.index]]$validate.function, x = model.list)
				tt <- eval(tt, sys.parent())
			}
		}
	}

	##
	##	take arguments and put them in fit.models object
	##

	else {

		n.models <- length(the.call) - 1
		mod.names <- as.character(unlist(the.call))[2:(n.models+1)]
		ans <- list()
		model.list <- list()
		for(i in 1:n.models) {
			ans[[i]] <- eval(the.call[[i + 1]], sys.parent(1))
			model.list[[i]] <- ans[[i]]$call
		}
		names(ans) <- names(model.list) <- mod.names

		models <- sapply(model.list, function(x) as.character(x[[1]]))
		if(is.null(models))
			stop("fit.models error 1")

		db.index <- -1

		for(i in 1:length(fmdb)) {
			temp <- match(models, fmdb[[i]]$classes, nomatch = 0)
			if(prod(temp) != 0) {
				db.index <- i
				break
			}
		}

		if(db.index == -1)
			stop("Specified model is not in the fit.models database.")

		attr(ans, "model.list") <- model.list
		oldClass(ans) <- fmdb[[db.index]]$object.class
	}

	ans
}




