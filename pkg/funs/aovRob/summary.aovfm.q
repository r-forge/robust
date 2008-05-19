summary.aovfm <- function(object, ...)
{

	append.p.values <- function(obj)
	{
		cbind(obj, 2 * (1 - pnorm(abs(obj[, ncol(obj)]))))
	}

	n.models <- length(object)
	model.list <- attr(object, "model.list")

	ans <- list()
	ans$mod.names <- mod.names <- format(names(model.list))
	ans$calls <- lapply(object, function(x) x$call)
	ans$aovtable <- lapply(object, summary)
	n.columns <- sapply(ans$aovtable, function(x) dim(x)[[2]])

	if(length(unique(n.columns)) != 1) {
			
		tmp.fun <- function(tmp) {
			nc <- dim(tmp)[[2]]
			tmp <- tmp[, c(1, nc - 1, nc), drop = F]
			tmp
		}

		ans$aovtable <- lapply(ans$aovtable, tmp.fun)
	}

	tmp.fun <- function(tmp) {
		tmpnames <- dimnames(tmp)[[1]]
		nnames <- length(tmpnames)
		if(any(strip.blanks(tmpnames) == "Residuals"))
			tmp <- tmp[-nnames, , drop = F]
		tmp
	}

	ans$aovtable <- lapply(ans$aovtable, tmp.fun)

	oldClass(ans) <- "summary.aovfm"
	ans
}


