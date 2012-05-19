"discRob" <- 
function(formula, data = sys.parent(), family = Classical(
	"homoscedastic"), frequencies, na.action = na.exclude,
	subset, prior = c("proportional", "uniform", "none"), method = 
	c("svd", "choleski"), singular.tol = sqrt(.Machine$
	double.eps), estim = "auto", cov.control =
	covRob.control(estim = estim), ...)
{

	integer.scaling <- function(n, contrasts = T)
	{
		if(is.numeric(n) && length(n) == 1) {
			x <- 1:n
			levs <- x
		}
		else {
			levs <- n
			n <- length(n)
			x <- 1:n
		}
		return(array(x, c(n, 1), list(levs, NULL)))
	}

	if( (family$family != "Classical") || ((family$cov.structure 
			!= "homoscedastic") && (family$cov.structure != 
			"heteroscedastic") ) ) 
			stop(paste("\nFamily '", family$family,
				"(", family$cov.structure, ")' not implemented.", 
				sep = ''))

	theCall <- match.call()
	method <- match.arg(method)
	if(length(theCall$formula) != 3)
		stop(paste("formula", x, "is incomplete"))
	# build call to model.frame. 
	m <- call("model.frame", formula = formula, data = substitute(
		data))
	if((k <- match("frequencies", names(theCall), nomatch = 0)))
		m$frequencies <- theCall$frequencies
	if((k <- match("subset", names(theCall), nomatch = 0))) {
		m$subset <- theCall$subset
		m$drop.unused.levels <- T
	}
	if((k <- match("na.action", names(theCall), nomatch = 0))) {
		m$na.action <- theCall$na.action
		m$drop.unused.levels <- T
	}
	data <- eval(m, local = sys.parent())
	Terms <- terms(data)
	# Make sure formula references vars explicitly (no dot in formula)
	theCall$formula <- formula(Terms)
	g <- model.extract(data, "response")
	if(is.null(g))
		stop("need a grouping variable on the left-hand-side of the formula"
			)
	if(!inherits(g, "factor")) {
		if(is.numeric(g)) {
			if(storage.mode(g) != "integer")
				stop("grouping variable must be a factor"
					)
			g <- paste("G", g, sep = "")
		}
		warning(paste(
			"converting the group variable to a factor"))
		g <- as.factor(g)
	}
	lev <- levels(g)
	ng <- length(lev)
	if(ng == 1)
		warning("There is only one level in groups factor.")
	vars <- attr(Terms, "term.labels")
	is.factor.var <- sapply(data[, vars, drop = F], is.factor)
	m <- call(as.name("model.matrix.default"), terms.object = 
		as.name("Terms"), data = as.name("data"))
	dots <- list(...)
	contr <- NULL
	if(any(is.factor.var)) {
		factor.vars <- vars[is.factor.var]
		contr <- structure(vector(mode = "list", length = 
			length(factor.vars)), names = factor.vars)
		if((k <- match("contrasts", names(dots), nomatch = 0))
			) {
			for(v in names(dots[[k]])) {
				contr[[v]] <- dots[[k]][[v]]
				if((j <- match(v, factor.vars, nomatch
					 = 0)))
					factor.vars <- factor.vars[
						 - j]
			}
			dots <- dots[ - k]
		}
		for(v in factor.vars) {
			#      do not use the contrast<- assigment operator; it strips out redundant columns
			contr[[v]] <- if(is.ordered(data[, v])) 
					integer.scaling(length(levels(
					data[, v]))) else 
					contr.treatment(length(levels(
					data[, v])))
		}
		m$contrasts.arg <- contr
	}
	attr(Terms, "response") <- 0
	X <- eval(m)
	#  remove intercept by hand to prevent model.matrix over parameterizing factors
	if(attr(Terms, "intercept")) {
		attr(Terms, "intercept") <- 0
		X <- X[, -1, drop = F]
	}
	#   save the indicies of the factor variables
	factor.indicies <- if(any(is.factor.var)) attr(X, "assign")[
			vars[is.factor.var]] else NULL
	n <- nrow(X)
	p <- ncol(X)
	# dont get varnames from Terms object since there may be factors on the RHS of formula 
	var.names <- dimnames(X)[[2]]
	group.means <- matrix(0, nrow = ng, ncol = p, dimnames = list(
		lev, var.names))
	Cov <- structure(vector(mode = "list", length = ng), names = 
		lev)
	scaling <- structure(vector(mode = "list", length = ng), names
		 = lev)
	Cov.p <- matrix(0, nrow = p, ncol = p, dimnames = list(
		var.names, var.names))
	v <- 0
	ldet <- structure(vector(mode = "numeric", length = ng), names
		 = lev)
	vi <- vector(mode = "numeric", length = ng)
	swf <- structure(.Data = rep(0, ng), .Names = lev)
	frequencies <- model.extract(data, "frequencies")
	if(!is.null(frequencies)) {
		if(length(frequencies) != n)
			stop(paste("length of the frequencies", length(
				frequencies), "must be", n))
		if(any(frequencies <= 0))
			stop("frequencies must all be positive")
	}
	else {
		frequencies <- rep(1, n)
	}
	if(!is.null(frequencies))
		counts <- tapply(frequencies, g, sum)
	else counts <- as.vector(table(g))
	names(counts) <- lev
	N <- sum(counts)
	if(length(counts) == length(g)) {
		stop(paste("all elements of the group variable", lhs,
			"are unique"))
	}
	if(any(i <- (counts < p + 1)))
		warning(paste("Group(s)", levels(g)[i], 
			"have counts less than the number of variables=",
			p))
	#  Begin Computations   
	is.spherical <- (family$cov.structure == "spherical" || family$
		cov.structure == "group spherical")
	if(is.spherical) {
		if(method != "choleski")
			warning(paste(
				"using choleski decomposition when covariance structure is ",
				family$cov.structure))
		method <- "choleski"
		theCall$method <- method
	}
	if(!is.null(prior)) {
		if(is.character(prior)) {
			prior <- match.arg(prior)
			switch(prior[1],
				proportional = prior <- counts/N,
				uniform = prior <- structure(.Data = 
					rep(1/length(counts), length(
					counts)), names = names(counts)
					),
				none = prior <- NULL,
				stop(paste("invalid prior", prior)))
		}
		else if(!is.numeric(prior) || length(prior) != length(
			counts) || any(prior < 0) || round(sum(prior),
			5) != 1) {
			stop("invalid prior")
		}
		else if(!is.null(names(prior))) {
			k <- match(names(prior), names(counts), nomatch
				 = 0)
			if(!all(k))
				stop(paste("names of priors ", names(
					prior), 
					"do not match group levels",
					levels(g)))
			prior <- prior[k]
		}
		else {
			names(prior) <- names(counts)
		}
	}
	v <- 0
	for(i in 1:ng) {
		use <- oldUnclass(g) == i
		fi <- frequencies[use]
		nk <- counts[i] - 1
		Xi <- X[use,  , drop = F]
		swf[i] <- sum(fi)
		vi[i] <- nk
		tmp <- covRob(sqrt(fi)*Xi, estim = estim, control = cov.control)
		Cov[[i]] <- tmp$cov
		tmp <- covRob(fi*Xi, estim = estim, control = cov.control)
		group.means[i, ] <- tmp$center
		Xi <- sqrt(fi) * scale(Xi, group.means[i, ], F)
		if(is.spherical && p > 1)
			Cov[[i]] <- diag(diag(Cov[[i]]), p)
		Cov.p <- Cov.p + nk * Cov[[i]]
		di <- sqrt(diag(Cov[[i]]))
		Si <- diag(1/di, p)
		if(any(k <- (abs(di) < .Machine$double.xmin))) {
			di <- di[!k]
			k <- seq(p)[k]
			Si[cbind(k, k)] <- 0
		}
		v <- v + nk
		switch(method,
			svd = {
				qx <- svd(Cov[[i]])
				r <- sum(qx$d > singular.tol * qx$
					d[1])
				scaling[[i]] <- (qx$v[,
					1:r, drop = F] %*% sqrt(diag(1/
					qx$d[1:r], r)))
				if(r < p)
					scaling[[i]] <- scaling[[i]] %*%
						t(qx$v[, 1:r, drop = F]
						)
				ldet[i] <- 2 * sum(log(qx$d[1:r])) -
					r * log(nk) + 2 * sum(log(
					di))
			}
			,
			choleski = {
				qx <- choleski(Cov[[i]], eps = 
					singular.tol)
				r <- qx$rank
				scaling[[i]] <- matrix(0, nrow = p,
					ncol = p)
				pivot <- qx$pivot
				scaling[[i]][pivot[1:r], pivot[1:r]] <-
					backsolve(qx$R[1:r, 1:r, drop
					 = F], diag(r))
				ldet[i] <- 2 * sum(log(diag(qx$R)[
					1:r]))
			}
			)
		if(r < p)
			warning(paste("Rank deficiency in group", lev[
				i]))
		dimnames(scaling[[i]]) <- list(var.names, paste(
			var.names, "'", sep = ""))
		attr(scaling[[i]], "rank") <- r
		X[use,  ] <- Xi
	}
	Cov.p <- Cov.p/v
	di <- sqrt(diag(Cov.p))
	Si <- diag(1/di, p)
	if(any(k <- (abs(di) < .Machine$double.xmin))) {
		di <- di[k]
		k <- seq(p)[k]
		Si[cbind(k, k)] <- 0
	}
	switch(method,
		svd = {
			qx <- svd(Cov.p)
			r <- sum(qx$d > singular.tol * qx$d[1])
			scaling.p <- (qx$v[, 1:r, drop = F] %*%
				sqrt(diag(1/qx$d[1:r], r)))
			if(r < p)
				scaling.p <- scaling.p %*% t(qx$v[
					, 1:r, drop = F])
			ldet.p <- 2 * sum(log(qx$d[1:r])) - r * log(
				v) + 2 * sum(log(di))
		}
		,
		choleski = {
			qx <- choleski(Cov.p, eps = singular.tol)
			r <- qx$rank
			scaling.p <- matrix(0, nrow = p, ncol = p)
			pivot <- qx$pivot
			scaling.p[pivot[1:r], pivot[1:r]] <- backsolve(
				qx$R[1:r, 1:r, drop = F], diag(r))
			ldet.p <- 2 * sum(log(diag(qx$R)[1:r]))
		}
		)
	if(r < p)
		warning("Rank deficiency in pooled covariance")
	dimnames(scaling.p) <- list(var.names, paste(var.names, "'",
		sep = ""))
	attr(scaling.p, "rank") <- r
	if(length(dots))
		family$fitter.call <- as.call(append(family$fitter.call,
			dots))
	fit <- eval(family$fitter.call)
	if(!is.null(prior))
		fit$coefficients$constants <- fit$coefficients$
			constants + log(prior)
	result <- append(list(call = theCall, family = family, counts
		 = counts, swf = swf, method = method), fit)
	if(!is.null(prior))
		result$prior <- prior
	result$variables <- vars
	if(any(is.factor.var))
		result$factors <- list(variables = vars[is.factor.var],
			contrasts = contr, levels = sapply(data[, vars[
			is.factor.var], drop = F], levels), indicies = 
			factor.indicies)
	oldClass(result) <- "discRob"
	result
}

summary.discRob <- function(object, MC = F, n.MC = 1000, ...)
{
	 error.table <- function(pred, groups, prior, counts)
	 {
	 	tbl <- if(!is.null(pred$frequencies)) table(rep(groups,
	 			pred$frequencies), rep(pred$groups,
	 			pred$frequencies)) else table(groups,
	 			pred$groups)
	 	lev <- levels(groups)
	 	ng <- length(lev)
	 	dt <- diag(tbl)
	 	err <- (rowSums(tbl) - dt)/counts
	 	dm <- dim(pred)
	 	post.err <- rep(0, ng)
	 	strat.err <- prior
	 	if(!is.null(prior) && all(round(counts/sum(counts),
	 		5) == round(prior, 5)))
	 		strat.err <- NULL
#	 	vars <- make.names(names(counts))
		vars <- names(pred)[-1]
	 	for(i in 1:ng) {
	 		use <- pred$groups == lev[i]
	 		x <- pred[use, vars[i]]
	 		if(!is.null(pred$frequencies))
	 			x <- x * pred$frequencies[use]
	 		post.err[i] <- sum(x)
	 		if(!is.null(strat.err))
	 			strat.err[i] <- sum(x * (prior/counts)[
	 				as.integer(groups)][use])
	 	}
	 	if(is.null(prior)) {
	 		post.err <- 1 - c(post.err/counts, Overall = 
	 			sum(post.err)/sum(counts))
	 		prior <- rep(1, ng)
	 	}
	 	else {
	 		post.err <- 1 - c(post.err/prior, Overall = sum(
	 			post.err))/sum(counts)
	 		if(!is.null(strat.err))
	 			strat.err <- 1 - c(strat.err/prior,
	 				Overall = sum(strat.err))
	 	}
	 	dm <- dimnames(tbl)
		tbl <- rbind(tbl, Overall = rep(NA, ng))
	 	err <- c(err, overall = sum(err * prior))
	 	tbl <- cbind(tbl, Error = err, Posterior.Error = 
	 		post.err)
	 	if(!is.null(strat.err))
	 		tbl <- cbind(tbl, Stratified.Error = strat.err)
	 	attr(tbl, "method") <- attr(pred, "method")
	 	tbl
	 }
	dist <- NULL
	ng <- dim(object$means)[1]
	p <- dim(object$means)[2]
	N <- sum(object$counts)
	v <- N - ng
	fam <- family(object)
	d <- model.frame(object)
	groups <- response(d)
	frq <- model.extract(d, "frequencies")
	lev <- levels(groups)
	Terms <- terms(d)
	vars <- attr(Terms, "term.labels")
	X <- model.matrix(Terms, data = d)
	#  clean up 
	remove("d", frame = sys.nframe())
	if(k <- attr(Terms, "intercept"))
		X <- X[,  - k, drop = F]
	switch(fam$model.type,
		linear = {
			dist <- matrix(0, nrow = ng, ncol = ng, 
				dimnames = list(lev, lev))
			f <- sqrt(N - ng)
			for(i in 1:ng) {
				use <- oldUnclass(groups) == i
				X[use,  ] <- scale(X[use,  , drop = F],
					object$means[i,  ], F) %*% 
					object$scaling
				dist[i,  ] <- rowSums((scale(object$
					means, object$means[i,  ],
					F) %*% object$scaling)^2)
			}
		}
		,
		quadratic = {
			dist <- matrix(0, nrow = ng, ncol = ng, 
				dimnames = list(lev, lev))
			f <- sqrt(object$counts - 1)
			for(i in 1:ng) {
				use <- oldUnclass(groups) == i
				X[use,  ] <- scale(X[use,  ], object$
					means[i,  ], F) %*% object$
					scaling[[i]]
				dist[, i] <- rowSums((scale(object$means,
					object$means[i,  ], F) %*% 
					object$scaling[[i]])^2)
			}
		}
		)

	##  clean up before calling predict 
	remove(c("X", "frq"), frame = sys.nframe())

	pred <- predict(object, ...)

	result <- list(discrim = object, classify = error.table(pred,
		groups, object$prior, object$counts))

	set.seed(1313)
	var.names <- attr(attr(model.frame(object), "terms"), "term.labels")

	if(MC) {
	  cat(paste("\nStarting simulation. Generating ", n.MC, 
	  			" new random samples.", sep = ""))
	  cat(paste("\nThis may take a few minutes."))
	  result$rule.MC <- array(NA, dim= c(NULL, ng))
	  dimnames(result$rule.MC) <- list(paste("Group ", c(lev), sep = ""))
	  hetero <- is.list((tmp <- object$covariances))
	  if(!hetero) 
	  		cov1 <- tmp
	  for(i in 1:ng) {
	  	if( hetero )
			cov1 <- object$covariances[[i]]
		mean1 <- object$means[i,]
		new.data <- rmvnorm(n.MC, mean = mean1, cov = cov1)
		dimnames(new.data) <- list(NULL, var.names)
		new.data <- as.data.frame(new.data)
		result$rule.MC[i] <- 
			sum(predict(object, new.data)$groups != groups[i]) / n.MC
	  }
	  cat("\nDone.\n")
	}

	if(!is.null(dist))
		result$distances <- dist
	oldClass(result) <- "summary.discRob"
	result
}


"print.summary.discRob" <- 
function(x, ...)
{
	print.upper <- function(x, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xx <- format(zapsmall(xx, .Options$
					digits))
				if(i < d[2])
					xx[(i + 1):d[2]] <- ""
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
				xx[!nas] <- format(zapsmall(round(
					xx[!nas], .Options$digits)))
				if(any(nas))
					xx[nas] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	print(x$discrim, ...)
	if(!is.null(x$canonical.analysis)) {
		cat("\nCanonical Correlations:\n")
		print.it(x$canonical.analysis$canonical.correlations,
			...)
		cat("\nEigenvalues:\n")
		print.it(x$canonical.analysis$eigenvalues, ...)
	}
	if(!is.null(x$homogeneity.tests)) {
		cat("\nTests for Homogeneity of Covariances:\n")
		print.it(x$homogeneity.tests, ...)
		cat(attr(x$homogeneity.tests, "note"))
	}
	if(!is.null(x$means.test)) {
		cat("\nTests for the Equality of Means:\n")
		print(x$means.test, ...)
	}
	if(!is.null(x$hotelling.T2)) {
		print(x$hotelling.T2, ...)
	}
	fam <- family(x$discrim)
	if(!is.null(x$distances)) {
		if(fam$model.type == "linear") {
			cat("\nMahalanobis Distance:\n")
			print.upper(x$distances, ...)
		}
		else {
			#quadratic
			cat("\nPairwise Generalized Squared Distances:\n"
				)
			print(x$distances, ...)
		}
	}
	if(!is.null(x$KS.test)) {
		cat("\nKolmogorov-Smirnov Test for Normality:\n")
		print.it(x$KS.test, ...)
	}
	st <- attr(x$classify, "method")
	if(!is.null(st))
		cat(paste("\n", paste(casefold(substring(st, 1, 1),
			T), substring(st, 2, nchar(st)), sep = ""),
			" classification table:\n", sep = ""))
	else cat("\nClassification table:\n")
	print.it(x$classify, ...)
	cat("(from=rows,to=columns)\n")
	if(!is.null(x$optimal.error.rate)) {
		cat("\nOptimal Error Rate:\n")
		print(x$optimal.error.rate, ...)
	}
	if(!is.null(x$rule.MSE)) {
		cat(paste("\nRule Mean Square Error:", format(x$
			rule.MSE), 
			"\n(conditioned on the training data)\n"))
	}
	if(!is.null(x$rule.MC)) {
		cat("\nMonte Carlo Error Rates:\n")
		print(x$rule.MC)
		cat("(conditioned on the training data)\n")
 	}	
	if(!is.null(x$CV)) {
		cat("\nCross-validation table:\n")
		print.it(x$CV, ...)
		cat("(from=rows,to=columns)\n")
	}
	return(invisible(NULL))
}

plot.discRob <- plot.discrim


"predict.discRob" <- 
function(object, newdata, doubt = 0, digits = .Options$
	digits)
{
	which.is.max <- function(x, doubt)
	{
		k <- length(x)
		m <- max(x)
		if(m < doubt)
			return(k + 1)
		d <- (1:k)[x == m]
		if(length(d) > 1)
			d <- sample(d, 1)
		d
	}
	fam <- family(object)
	freq <- NULL
	if(missing(newdata)) {
		x <- model.frame(object)
		freq <- model.extract(x, "frequencies")
		Terms <- terms(x)
	}
	else {
		if(is.null(f <- object$call$formula))
			stop(paste("bad formula", object$call$formula))
		Terms <- terms(f)
		if(attr(Terms, "response") != 0) {
			if(match(deparse(f[[2]]), dimnames(newdata)[[2]], nomatch = 0) == 0)
				f[[2]] <- NULL
		}
		if(anyMissing(k <- match(l <- all.vars(parse(text = f)), dimnames(newdata)[[2]]))) {
			stop(paste("newdata is missing variable(s)", paste(l[is.na(k)], collapse = ", ")
				))
		}
		if(!is.null(nas <- object$call$na.action))
			x <- model.frame(f, newdata, na.action = nas)
		else x <- model.frame(f, newdata)
	}
	x <- model.matrix(Terms, x)
	if((k <- attr(Terms, "intercept")))
		x <- x[,  - k, drop = F]
	x <- as.matrix(x)
	if(dim(x)[2] != dim(object$means)[2])
		stop("wrong number of variables")
	if(length(dimnames(x)[[2]]) > 0 && any(dimnames(x)[[2]] != dimnames(object$means)[[2]]))
		warning("variable names in newdata do not match those in object")
	lev <- names(object$counts)
	ng <- length(lev)
	n <- dim(x)[1]
	p <- dim(x)[2]
	N <- sum(object$counts)
	lprior <- if(!is.null(object$prior)) log(object$prior) else rep(0, ng)
	cf <- coef(object)
	switch(fam$model.type,
		linear = {
					dist <- scale(x %*% cf$linear.coefficients,  - cf$constants,
						F)
				}
				,
		quadratic = {
					dist <- scale(x %*% cf$linear.coefficients,  - cf$constants,
						F) + as.matrix(as.data.frame(lapply(cf$
						quadratic.coefficients, function(w, x)
					{
						rowSums(x %*% w * x)
					}
					, x)))
				}
		,
		stop(paste("object ", deparse(substitute(object)), 
			"is missing type = \"linear\", or \"quadratic\"")))
	dist <- exp((dist - rowMaxs(dist, na.rm = T)))
	dist <- dist/drop(rowSums(dist))
	if(digits > 0)
		dist <- round(dist, digits)
	dimnames(dist) <- list(dimnames(x)[[1]], lev)
	groups <- apply(dist, 1, which.is.max, doubt = doubt + 1/length(lev))
	if(length(unique(groups)) == length(lev) + 1) {
		k <- 0
		lev1 <- "Unknown"
		while(match(lev1, lev, nomatch = 0)) {
			k <- k + 1
			if(k > 10)
				stop("could not generate unique level for doubt class")
			lev1 <- paste(rep("?", k), collapse = "")
		}
		lev <- c(lev, lev1)
	}
	# create the factor the hard way.  The factor() constructor burps if we have a group 
	#  that does not have any elements in it
	# groups <- factor(groups, labels = lev)
	groups <- structure(.Data = groups, levels = lev, class = "factor")
	result <- data.frame(groups = groups, row.names = dimnames(x)[[1]])
	if(!is.null(freq))
		result <- cbind(result, frequencies = freq)
	result <- cbind(result, dist)
	theCall <- match.call()
	theCall[[1]] <- as.name("predict")
	attr(result, "call") <- theCall
	return(result)
}



"crossvalidate.discRob" <- 
function(object, doubt = 0, digits = .Options$digits, trace = T, subset)
{
	which.is.max <- function(x, doubt)
	{
		k <- length(x)
		m <- max(x)
		if(m < doubt)
			return(k + 1)
		d <- (1:k)[x == m]
		if(length(d) > 1)
			d <- sample(d, 1)
		d
	}
	fam <- family(object)
	x <- model.frame(object)
	Terms <- terms(x)
	g <- model.extract(x, "response")
	frq <- model.extract(x, "frequencies")
		#  brute force 
		result <- NULL
		vars <- attr(Terms, "term.labels")
		syn.nms <- make.names(vars)
		m <- object$call
		if(!all(match(syn.nms, names(x), nomatch = 0))) {
			#   possible transformation occured
			k <- match(vars, names(x), nomatch = 0)
			if(!all(k))
				stop(paste("not all term labels '", paste(vars, collapse = ","), 
					"' are in the model.frame", sep = ""))
			names(x)[k] <- syn.nms
			f <- paste(names(x)[attr(Terms, "response")], "~", paste(syn.nms, collapse = "+"
				))
			m$formula <- formula(f)
			warning(paste("transformations made. Converted formula:", f))
		}
		m$data <- as.name("data")
		if(!is.null(object$prior))
			m$prior <- object$prior
		if(!is.null(frq))
			m$frequencies <- as.name("frq")
		#  already subsetted in model.frame   
		if(!is.null(m$subset)) m$subset <- NULL
		if(missing(subset))
			subset <- 1:nrow(x)
		k <- 0
		for(i in subset) {
			if(!is.null(frq)) {
				if(frq[i] > 1) {
					f <- frq
					f[i] <- f[i] - 1
					assign("frq", f, frame = 1)
					assign("data", x, frame = 1)
					remove("f", frame = sys.nframe())
				}
				else {
					assign("frq", frq[ - i], frame = 1)
					assign("data", x[ - i,  ], frame = 1)
				}
			}
			else {
				assign("data", x[ - i,  ], frame = 1)
			}
			fit <- eval(m)
			result <- rbind(result, predict(fit, newdata = x[i,  , drop = F]))
			k <- k + 1
			if(trace && k %% 10 == 0) {
				cat(paste("\nObservation", i, "\n"))
				if(!is.null(frq))
					print(table(rep(g[subset[1:k]], frq[subset[1:k]]), rep(result$
						groups, frq[subset[1:k]])))
				else print(table(g[subset[1:k]], result$groups))
			}
		}
		if(!is.null(frq))
			result$frequencies <- frq
		attr(result, "method") <- "crossvalidate"
		theCall <- match.call()
		theCall[[1]] <- as.name("crossvalidate")
		attr(result, "call") <- theCall
		return(result)
}


Cov.discRob <- 
function(object)
{
	covariances <- object$covariances
	attr(covariances, "cov.structure") <- family(object)$cov.structure
	oldClass(covariances) <- "Cov.discRob"
	return(covariances)
}


family.discRob <-
function(object)
{
	result <- object$family
	oldClass(result) <- "family.discRob"
	return(result)
}


print.family.discRob <-
function(x, ...)
{
	cat("Discriminant Family: ", x$family, "\n")
	cat("Covariance Structure: ", x$cov.structure, "\n")
	cat("Model type: ", x$model.type, "\n")
}


print.Cov.discRob <-
function(x, ...)
{
	print.upper <- function(x, digits = .Options$digits, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xx <- format(zapsmall(xx, digits = digits))
				if(i < d[2])
					xx[(i + 1):d[2]] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	cat(paste("\nCovariance Structure:", attr(x, "cov.structure"), "\n"))
	if(is.list(x)) {
		for(grp in names(x)) {
			cat(paste("\nGroup:", grp, "\n"))
			print.upper(x[[grp]], ...)
		}
	}
	else {
		print.upper(x, ...)
	}
	return(invisible(NULL))
}


print.discRob <-
function(x, short = T, ...)
{
	print.upper <- function(x, digits = .Options$digits, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xx <- format(zapsmall(xx, digits = digits))
				if(i < d[2])
					xx[(i + 1):d[2]] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	if(!is.null(cl <- x$call)) {
		names(cl)[2] <- ""
		cat("\nCall:\n")
		dput(cl)
	}
	tbl <- cbind(x$means, N = x$counts)
	if(!all(x$swf == x$counts))
		tbl <- cbind(tbl, Weights = x$swf)
	if(!is.null(x$prior))
		tbl <- cbind(tbl, Priors = x$prior)
	cat("\nGroup means:\n")
	print(tbl, ...)
	covar <- Cov(x)
	par <- parameters(x)
	fam <- family(x)
	if(!short) {
		print(covar, ...)
		print(par, ...)
	}
	else {
		switch(fam$cov.structure,
			"equal correlation" = {
				cat("\nCovariance Structure: equal correlation\n")
				D <- diag(1/sqrt(diag(covar[[1]])))
				Corr <- D %*% covar[[1]] %*% D
				dimnames(Corr) <- dimnames(covar[[1]])
				remove("D", frame = sys.nframe())
				cat("\nCommon Correlation Matrix:\n")
				print.upper(Corr, digits = 4)
				vars <- data.frame(lapply(covar, diag), row.names = dimnames(Corr)[[
					1]])
				cat("\nVariances:\n")
				print(vars, ...)
			}
			,
			proportional = {
				cat(paste("\nCovariance Structure:", attr(par, "type"), "\n"))
				if(fam$family == "Classical") {
					print.upper(covar[[1]], ...)
					cat("\nProportionality Constants:\n")
					print(par$K^2, ...)
				}
				else {
					print(par, ...)
				}
			}
			,
			"common principal component" = {
				cat("\nCovariance Structure: common principal component\n")
				print(par, ...)
			}
			,
			{
				print(covar, ...)
				print(par, ...)
			}
			)
	}
	print(coef(x), ...)
	return(invisible(x))
}



coef.discRob <- function(object)
{
	coefficients <- object$coefficients
	attr(coefficients, "type") <- family(object)$model.type
	oldClass(coefficients) <- c("coef.discRob")
	return(coefficients)
}



print.coef.discRob <- function(x, ...)
{
	print.it <- function(x, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				nas <- is.na(xx)
				xx[!nas] <- format(zapsmall(round(xx[!nas], .Options$digits)))
				if(any(nas))
					xx[nas] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	print.upper <- function(x, digits = .Options$digits, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xx <- format(zapsmall(xx, digits = digits))
				if(i < d[2])
					xx[(i + 1):d[2]] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	cat("\nConstants:\n")
	print(x$constants, ...)
	cat("\nLinear Coefficients:\n")
	print.it(x$linear.coefficients, ...)
	if(attr(x, "type") == "quadratic") {
		cat("\nQuadratic coefficents:\n")
		for(pop in names(x$quadratic.coefficients)) {
			cat(paste("\ngroup:", pop, "\n"))
			print.upper(x$quadratic.coefficients[[pop]], ...)
		}
	}
	return(invisible(NULL))
}
 

parameters.discRob <-
function(object)
{
	parameters <- object$parameters
	fam <- family(object)
	switch(fam$family,
		CPC = {
			attr(parameters, "type") <- if(fam$cov.structure == "proportional") 
					"CPC proportional" else "CPC"
		}
		,
		Classical = {
			attr(parameters, "type") <- fam$cov.structure
		}
		,
		Canonical = {
			attr(parameters, "type") <- "canonical"
		}
		)
	oldClass(parameters) <- "parameters.discRob"
	return(parameters)
}

print.parameters.discRob <-
function(x, ...)
{
	if(!is.null(x)) {
		switch(attr(x, "type"),
			CPC = {
				cat("\nPrincipal Components:\n")
				print(x$A, ...)
				cat("\nCharacteristic Roots:\n")
				print(x$L, ...)
			}
			,
			"CPC proportional" = {
				cat("\nCommon Principal Components:\n")
				print(x$A, ...)
				cat("\nCommon Characteristic Roots:\n")
				print(x$L, ...)
				cat("\nProportionality Constants:\n")
				print(x$K)
			}
			,
			proportional = {
				cat("\nProportionality Constants:\n")
				print(x$K, ...)
			}
			,
			"equal correlation" = {
				cat("\nK:\n")
				print(x$K, ...)
			}
			,
			canonical = {
				cat("\nCanonical Coefficients:\n")
				print(x$canonical.coefficients, ...)
				cat("\nSingular Values:\n")
				print(x$values, ...)
			}
			)
	}
	return(invisible(NULL))
}
 

multicomp.discRob <-
function(x, alpha = 0.05, significant.only = T, ...)
{
	ch <- function(x)
	{
		ch <- choleski(x)
		r <- ch$rank
		p <- dim(x)[1]
		if(r < p) {
			R <- matrix(0, nrow = p, ncol = p)
			R[ch$pivot[1:r], ch$pivot[1:r]] <- ch$R[1:r, 1:r]
		}
		else R <- ch$R
		return(R)
	}
	bounds <- "both"
	error.type <- "fwe"
	method <- "best.fast"
	valid.check <- T
	ng <- dim(x$means)[1]
	if(ng <= 1)
		return(NULL)
	p <- dim(x$means)[2]
	N <- sum(x$counts)
	var.labels <- dimnames(x$means)[[2]]
	lev <- dimnames(x$means)[[1]]
	labels <- paste(rep(lev[1:(ng - 1)], seq(from = (ng - 1), to = 1, by = -1)), lev[unlist(sapply(
		2:ng, function(x, ng)
	seq(x, ng), ng = ng))], sep = "-")
	nc <- (ng * (ng - 1))/2
	.F <- structure(vector(mode = "numeric", length = nc), names = labels)
	df1 <- p
	Pr <- rep(NA, length = nc)
	covar <- Cov(x)
	fam <- family(x)
	if(fam$model.type == "linear") {
		S <- x$scaling
		df2 <- rep(N - ng, length = nc)
	}
	else {
		Rs <- lapply(covar, ch)
		df2 <- rep(0, length = nc)
	}
	k <- 0
	lev.a <- abbreviate(lev, minlength = 8)
	var.a <- abbreviate(var.labels, minlength = 8)
	mcomp.labels <- vector(mode = "character", length = (ng * (ng - 1))/2)
	mcomp <- vector(mode = "list", length = (ng * (ng - 1))/2)
	for(i in 1:(ng - 1)) {
		for(j in (i + 1):ng) {
			crit.point <- NULL
			Srank <- NULL
			control <- NULL
			simsize <- NULL
			labels <- paste(paste(lev.a[i], var.a, sep = "."), paste(lev.a[j], var.a, sep = 
				"."), sep = "-")
			k <- k + 1
			mcomp.labels[k] <- paste(lev[i], lev[j], sep = ".vs.")
			del <- t(x$means[i,  , drop = F] - x$means[j,  , drop = F])
			if(fam$model.type == "linear") {
				nn <- (x$swf[i] * x$swf[j])/(x$swf[i] + x$swf[j])
				.F[k] <- nn * crossprod(t(S) %*% del)
				St <- covar/nn
			}
			else {
				St <- covar[[i]]/x$swf[i] + covar[[j]]/x$swf[j]
				cx <- choleski(St)
				S <- matrix(0, nrow = p, ncol = p)
				r <- cx$rank
				pivot <- cx$pivot
				S[pivot[1:r], pivot[1:r]] <- t(backsolve(cx$R[1:r, 1:r], diag(r)))
				delp <- crossprod(S) %*% del
				.F[k] <- t(del) %*% delp
				df2[k] <- (crossprod(Rs[[i]] %*% delp/sqrt(x$swf[i]))/.F[k])^2/(x$counts[
					i] - 1) + (crossprod(Rs[[j]] %*% delp/sqrt(x$swf[j]))/.F[k])^
					2/(x$counts[j] - 1)
				df2[k] <- 1/df2[k]
			}
			dfe <- df2[k]
			df2[k] <- df2[k] - p + 1
			.F[k] <- (.F[k] * df2[k])/p/dfe
			Pr[k] <- 1 - pf(.F[k], df1, df2[k])
			if(!significant.only || Pr[k] < alpha)
				mcomp[[k]] <- multicomp.default(del, St, lmat = diag(length(del)), 
					comparisons = "none", df.residual = df2[k], alpha = alpha, 
					bounds = bounds, error.type = error.type, method = method, 
					crit.point = crit.point, Srank = Srank, control = control, 
					simsize = simsize, plot = F, labels = labels, valid.check = 
					valid.check, ylabel = "")
		}
	}
	names(mcomp) <- mcomp.labels
	T2.table <- data.frame(F = .F, df1 = df1, df2 = df2, Pr = Pr)
	if(fam$model.type != "linear")
		attr(T2.table, "note") <- "* df2 is Yao's approximation."
	result <- list(hotellings.T2 = T2.table)
	use <- !unlist(lapply(mcomp, is.null))
	if(any(use))
		result$multiple.comparisons <- mcomp[use]
	oldClass(result) <- "multicomp.discRob"
	return(result)
}



print.multicomp.discRob <-
function(x, ...)
{
	print.it <- function(x, digits = .Options$digits, ...)
	{
		x <- as.data.frame(x)
		d <- dim(x)
		for(i in 1:d[2]) {
			xx <- x[[i]]
			if(!length(levels(xx)) && is.numeric(xx)) {
				xna <- is.na(xx)
				xx <- format(zapsmall(xx, digits = digits))
				xx[xna] <- ""
				x[[i]] <- xx
			}
		}
		print(x, ...)
		return(invisible(NULL))
	}
	print.it2 <- function(x, digits = 3, ...)
	{
		#   taken from print.multicomp  
		labels <- dimnames(x$table)[[1]]
		estimate <- signif(x$table[, "estimate"], digits)
		stderr <- signif(x$table[, "stderr"], digits)
		flaggit <- ifelse((x$table[, "lower"] > 0 | x$table[, "upper"] < 0), "****", "    ")
		lower <- signif(x$table[, "lower"], digits)
		upper <- signif(x$table[, "upper"], digits)
		table <- data.frame(estimate, stderr, lower, upper, flaggit)
		names(table) <- c("Estimate", "Std.Error", "Lower Bound", "Upper Bound", "")
		row.names(table) <- labels
		print(table)
		cat("(critical point:", round(x$crit.point, 4), ")\n")
		if(x$method == "scheffe")
			cat("(rank used for Scheffe method:", as.character(x$Srank), ")\n")
		if(x$method == "sim")
			cat("(simulation size=", as.character(x$simsize), ")\n")
		return(invisible(NULL))
	}
	cat("\nHotelling's T Squared for Differences in Means Between Each Group:\n")
	print.it(x$hotellings.T2, ...)
	if(!is.null(attr(x$hotellings.T2, "note")))
		cat(paste(attr(x$hotellings.T2, "note"), "\n"))
	if(!is.null(x$multiple.comparisons)) {
		#  may not have any comparisions if none of the T tests are significant
		method <- switch(x$multiple.comparisons[[1]]$method,
			lsd = "Fisher LSD",
			tukey = "Tukey",
			dunnett = "Dunnett",
			sidak = "Sidak",
			bon = "Bonferroni",
			scheffe = "Scheffe",
			sim = "simulation-based",
			"user-specified" = "user-specified")
		cat(paste("\n", format(100 * (1 - x$multiple.comparisons[[1]]$alpha)), "% ", 
			"Simultaneous Confidence Intervals Using the ", method, " Method:\n", sep = ""))
		lapply(x$multiple.comparisons, print.it2)
		cat("* Intervals excluding 0 are flagged by '****'\n")
	}
	return(invisible(NULL))
}


model.frame.discRob <-
function(formula, data = NULL, ...)
{
	#  formula is acually a discrim object
	if(is.null(f <- formula$call$formula)) stop(paste("bad formula", formula$call$formula))
	Terms <- terms(f)
	if(!is.null(data)) {
		m <- match.call()
		m[[1]] <- as.name("model.frame.default")
		m$formula <- f
		if(attr(Terms, "response") != 0) {
			if(match(deparse(f[[2]]), dimnames(data)[[2]], nomatch = 0) == 0)
				f[[2]] <- NULL
		}
		if(anyMissing(k <- match(l <- all.vars(parse(text = f)), dimnames(data)[[2]]))) {
			stop(paste("data is missing variable(s)", paste(l[is.na(k)], collapse = ", ")))
		}
	}
	else {
		theCall <- formula$call
		#  Build call to model.frame. Cannot start with theCall as model.frame.default 
		#    generates an error for arguments not intendend for it (such as tol=xxx).
		m <- call("model.frame.default", formula = theCall$formula)
		if((k <- match("data", names(theCall), nomatch = 0)))
			m$data <- theCall$data
		if((k <- match("frequencies", names(theCall), nomatch = 0)))
			m$frequencies <- theCall$frequencies
		if((k <- match("subset", names(theCall), nomatch = 0))) {
			m$subset <- theCall$subset
			m$drop.unused.levels <- T
		}
		if((k <- match("na.action", names(theCall), nomatch = 0))) {
			m$na.action <- theCall$na.action
			m$drop.unused.levels <- T
		}
	}
	data <- eval(m, local = sys.parent())
	Terms <- terms(data)
	vars <- attr(Terms, "term.labels")
	is.factor.var <- sapply(data[, vars, drop = F], is.factor)
	if(!is.null(formula$factors)) {
		fact <- names(formula$factors$contrasts)
		if(sum(is.factor.var) != length(fact) || !all(match(fact, vars[is.factor.var], nomatch
			 = 0)))
			stop("factors used in the discriminant function do not match those in the origina
l data"
				)
		for(f in fact) {
			attr(data[, f], "contrasts") <- formula$factors$contrasts[[f]]
		}
	}
	iresp <- attr(Terms, "response")
	if(iresp > 0 && iresp < ncol(data) && !is.factor(data[, iresp])) {
		if(is.numeric(data[, iresp])) {
			if(storage.mode(data[, iresp]) != "integer")
				stop("grouping variable must be a factor")
			data[, iresp] <- paste("G", data[, iresp], sep = "")
		}
		data[, iresp] <- as.factor(data[, iresp])
	}
	return(data)
}

anova.discRob <- function(object) {

stop("The anova method is not yet implemented\nfor objects of class discRob")

return(invisible(object))

}
