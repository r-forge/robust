print.summary.aovfm <- function(x, ...)
{
	n.models <- length(x$calls)
	digits <- options()$digits - 3

	coln <- unique(strip.blanks(unlist(lapply(x$aovtable,
							function(x) dimnames(x)[[1]]))))
	np <- length(coln)
	colnames <- matrix(rep(coln, n.models), nrow = n.models, byrow = T)
	colnames <- paste(x$mod.names, colnames)
		n.columns <- dim(x$aovtable[[1]])[[2]]
	tmp <- matrix(NA, ncol = n.columns, nrow = length(colnames))
	dimnames(tmp) <- list(colnames, dimnames(x$aovtable[[1]])[[2]])
	for(i in 1:n.models) {
	  idx <- match(strip.blanks(dimnames(x$aovtable[[i]])[[1]]), coln)
	  tmp[n.models*(idx-1)+i, ] <- as.matrix(x$aovtable[[i]])
	}

	cat("\nComparison of ANOVA Tables:\n")
	tmp.idx <- is.na(tmp)
	tmp[, 2:n.columns] <- format(round(tmp[, 2:n.columns], digits = digits))
	tmp[tmp.idx] <- ""
	print(tmp, quote = F, right = T, ...)

	invisible(x)
}


