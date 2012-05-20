

lmasmRob <- function(formula, data, subset, error.distribution = "gaussian",
							control = lmasmRob.control())

{
	fun.call <- match.call()

	m <- call("model.frame", formula = fun.call$formula, data = fun.call$data)
	if(!missing(subset))
		m$subset <- substitute(subset)

	m <- eval(m, sys.parent())

	Terms <- attr(m, "terms")
  y <- model.extract(m, response)
  X <- model.matrix(Terms, m, contrasts = NULL)
	variable.names <- dimnames(X)[[2]]

	initial <- control$initial
	otp <- control$otp
	cov <- control$cov
	cu <- control$cu
	input <- control$input
	iv <- control$iv
	nrep <- control$nrep
	seed <- control$seed
	maxit <- control$maxit
	tol <- control$tol
	gam <- control$gam
	nitmon <- control$nitmon

	ans <- list()
	ans$robust.control <- control
	ans$contrasts <- NULL
	ans$terms <- Terms
	ans$call <- fun.call
	ans$assign <- attr(m, "assign")
	ans$est <- "final"
	ans$rank <- p <- ncol(X)

	switch(error.distribution,

		logweibull = {
			object <- TML.logweibull(X, log(y), initial = initial,
				otp = otp, cov = cov, cu = cu, input = input, iv = iv,
				nrep = nrep, seed = seed, maxit = maxit, tol = tol,
				gam = gam, nitmon = nitmon)

		},

		gaussian = {
			object <- TML.gauss(X, y, initial = initial, otp = otp,
				cov = cov, cu = cu, input = input, iv = iv,
				nrep = nrep, seed = seed)
			ans$coefficients <- object$th1
			ans$scale <- object$v1
			ans$fitted.values <- as.vector(X %*% matrix(ans$coefficients, ncol = 1))
			ans$residuals <- y - ans$fitted.values
			ans$M.weights <- object$wi
			ans$cov <- object$CV1[1:p, 1:p]
			dimnames(ans$cov) <- list(variable.names, variable.names)
			ans$robust.control$final.alg <- "TML.gauss"
		},

		lognormal = {

		},

		stop(paste("\"", error.distribution, 
			"\" is not a valid error distribution.", sep = ""))

	)

	oldClass(ans) <- "lmasmRob"
	ans
}

lmasmRob.control <- function(initial = "S", otp = "fixed", cov = "parametric",
	cu = 1.855356, input = NULL, iv = 1, nrep = 0, seed = 1313,
	maxit = 100, tol = 0.0001, gam = 0.1, nitmon = F)

{
	list(initial = initial, otp = otp, cov = cov, cu = cu,
		input = input, iv = iv, nrep = nrep, seed = seed,
		maxit = maxit, tol = tol, gam = gam, nitmon = nitmon)
}
