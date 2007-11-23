summary.aovRob <- function(object, ...)
{
  ss <- anova.lmRob(object, test="RF")
  attr(ss, "heading") <- NULL
	if(object$balanced) {
		cf.ss <- dummy.coef(object)

		if(!is.null(cf.ss[["(Intercept)"]])) {
			cf.ss[["(Intercept)"]] <- NULL
			intercept <- 1
		}
		else
			intercept <- 0

		terms <- names(cf.ss)

		term.sets <- list()
		n.levels <- list()
		dtf <- list()

		n <- length(object$residuals)

		for(term in terms) {
			term.sets[[term]] <- unlist(strsplit(term, split = ":"))
			n.levels[[term]] <- length(cf.ss[[term]])
			dtf[[term]] <- prod(unlist(n.levels[term.sets[[term]]]) - 1)
			cf.ss[[term]] <- sum(cf.ss[[term]]^2)
			cf.ss[[term]] <- (n / n.levels[[term]]) * cf.ss[[term]]
		}

	ss <- data.frame(ss[[1]],c(NA, unlist(cf.ss)), c(NA,
    unlist(cf.ss))/ss[[1]],ss[[2]],ss[[3]])

	names(ss) <- c("Df", "Sum of Sq", "Mean Sq", "RobustF", "Pr(F)")
	}
  ss[-1, , drop = FALSE]
}

