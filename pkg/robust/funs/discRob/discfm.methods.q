print.discfm <- function(x, ...)
{
	fun.call <- match.call()
	n.models <- length(x)

	model.list <- attr(x, "model.list")
	mod.names <- format(names(model.list))

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	l.coefs <- lapply(x, coef)
	cat("\nConstants:\n")

	for(j in 1:n.models) {
			cat(paste("\n", mod.names[j], "\n", sep = ""))
			print(l.coefs[[j]][[1]])
	}
	
	cat("\nLinear Coefficients:\n")
	for(j in 1:n.models) {
			cat(paste("\n", mod.names[j], "\n", sep = ""))
			print(l.coefs[[j]][[2]])
	}

	if(attr(l.coefs[[1]], "type") == "quadratic") {
		cat("\nQuadratic Coefficients:\n")
		for(pop in names(l.coefs[[1]][[3]])) {
			cat(paste("\nGroup:", pop, "\n"))
			for(j in 1:n.models) {
				cat(paste("\n", mod.names[j], "\n", sep =""))
				print(l.coefs[[j]][[3]][[pop]])
			}
		}
	}

	invisible(x)
}
	

summary.discfm <- function(object, MC = F, n.MC = 1000, ...)
{
	fun.call <- match.call()

	n.models <- length(object)
	model.list <- attr(object, "model.list")

	ans <- list()
	ans$mod.names <- mod.names <- format(names(model.list))
	ans$calls <- lapply(object, function(x) x$call)

	ans$distances <- ans$classify <- ans$rule.MC <- list() 
	for(i in 1:n.models) {
		tmp <- summary.discRob(object[[i]], MC = T, ...)
		ans$distances[[i]] <- tmp$distances
		ans$classify[[i]] <- tmp$classify
		ans$rule.MC[[i]] <- tmp$rule.MC
	}

	ans$discrim <- object
	oldClass(ans) <- "summary.discfm"
	ans
}


print.summary.discfm <- function(x, ...)
{
	print.upper <- function(x, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xx <- format(zapsmall(xx, .Options$digits))
				if(i < d[2]) xx[(i + 1):d[2]] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}

	print.it <- function(x, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				nas <- is.na(xx)
				xx[!nas] <- format(zapsmall(round(xx[!nas], .Options$digits)))
				if(any(nas)) xx[nas] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}

	n.models <- length(x$calls)
	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(x$mod.names[i], ": ")
		print(x$calls[[i]])
	}

	l.coefs <- lapply(x$discrim, coef)
	cat("\nConstants:\n")
	for(j in 1:n.models) {
		cat(paste("\n", x$mod.names[j], "\n", sep = ""))
		print(l.coefs[[j]][[1]])
	}

	cat("\nLinear Coefficients:\n")
	for(j in 1:n.models) {
		cat(paste("\n", x$mod.names[j], "\n", sep = ""))
		print(l.coefs[[j]][[2]])
	}

	if(attr(l.coefs[[1]], "type") == "quadratic") {
		cat("\nQuadratic Coefficients:\n")
		for(pop in names(l.coefs[[1]][[3]])) {
			cat(paste("\nGroup:", pop, "\n"))
			for(j in 1:n.models) {
				cat(paste("\n", x$mod.names[j], "\n", sep =""))
				print(l.coefs[[j]][[3]][[pop]])
			}
		}
	}

	fam <- family(x$discrim[[1]])

	if(!is.null(x$distances)) {
		if(fam$model.type == "linear") {
			cat("\nMahalanobis Distances:\n")
			for(j in 1:n.models) {
				cat(paste("\n", x$mod.names[j], "\n", sep = ""))
				print.upper(x$distances[[j]])
			}
		}

		else { #quadratic
			cat("\nPairwise Generalized Squared Distances:\n")
			for(j in 1:n.models) {
				cat(paste("\n", x$mod.names[j], "\n", sep = ""))
				print(x$distances[[j]])
			}
		}
	}

	st <- attr(x$classify[[1]], "method")

	if(!is.null(st))
		cat(paste("\n", paste(casefold(substring(st, 1, 1), T),
			substring(st, 2, nchar(st)), sep = ""),
			" classification table:\n", sep = ""))

	else
		cat("\nClassification table:\n")

	for(j in 1:n.models) {
		cat(paste("\n", x$mod.names[j], "\n", sep = ""))
		print.it(x$classify[[j]])
	}

		cat("(from=rows,to=columns)\n")

	if(!is.null(x$rule.MC)) {
		cat("\nMonte Carlo Error Rates:\n")
		rule.MC <- matrix(NA, nrow = n.models, ncol = length(x$rule.MC[[1]]))
		dimnames(rule.MC) <- list(rep(" ", n.models), dimnames(x$rule.MC[[1]])[[1]])
		for(j in 1:n.models) {
			dimnames(rule.MC)[[1]][j] <- x$mod.names[j]
			rule.MC[j, ] <- c(x$rule.MC[[j]])
		}
		print(rule.MC)
		cat("\n(conditioned on the training data)\n")
	}

	invisible(x)
}
