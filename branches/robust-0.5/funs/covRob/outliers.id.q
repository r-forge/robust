outliers.id <- function(x, frac.train = 1.0, variables = NULL,
	threshold = .975, compact = F, include.data = T,
	targets = NULL, warn.p = T, filename = NULL,
	control = covRob.control())
{
	if(!is.null(targets) && class(x) == "data.frame") {
		outliers <- by(x, x[targets], outliers.id, frac.train = frac.train,
			threshold = threshold, compact = compact, include.data = include.data,
			control = control, warn.p = F)

		if(!is.null(filename)){
			array.names <- dimnames(outliers)[[targets]]
			the.names <- c(targets, names(outliers[[1]]))

			if(length(array.names) == 1) {
				outliers <- data.frame(array.names, outliers)
				names(outliers) <- the.names
				write.table(outliers, filename)
			}
			else {
				ans <- data.frame(array.names[1], outliers[[1]])
				names(ans) <- the.names
				for(i in 2:length(array.names)) {
					outliers[[i]] <- data.frame(array.names[i], outliers[[i]])
					names(outliers[[i]]) <- the.names
					ans <- rbind(ans, outliers[[i]])
				}
			write.table(ans, filename)
			}
		}
		return(outliers)
	}

	if(class(x) == "covRob")
		data <- eval(x$call$data, sys.parent())

	else if(class(x) == "data.frame") {

		factors <- names(x)[sapply(x, is.factor)]

		message <- NULL
		if(length(factors)) {
			message <- paste("factor column(s) dropped: ",
				paste(factors, collapse = ", "), ",", sep = "")
			factors <- match(factors, names(x))
			x <- x[-factors, drop = F]
		}

		if(!is.null(variables))
			x <- data <- x[variables]
		else data <- x
		n <- nrow(x)
		train <- as.integer(frac.train * n)
		x <- covRob(x[1:train,], control = control, distance = F)
		x$dist <- mahalanobis(data, x$center, x$cov)
	}

	else stop(paste("\"", deparse(substitute(x)),
		"\" is not a data.frame or a covRob object.", sep = ""))

	if(!is.null(x$dist))
		dists <- x$dist
	else
		dists <- mahalanobis(data, x$center, x$call)

	p <- dim(x$cov)[1]
	cp <- qchisq(threshold, p)
	outliers <- as.integer(dists > cp)

	outliers <- data.frame(Distances = dists, Outliers = outliers)

	if(include.data)
		outliers <- cbind(data, outliers)
	else
		row.names(outliers) <- row.names(data)

	if(compact)
		outliers <- outliers[as.logical(outliers$Outliers),]

	if(!is.null(message) && warn.p)
		warning(message)

	if(!is.null(filename))
		write.table(outliers, file = filename)

	outliers
}
