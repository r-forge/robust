# menuGamRob

menuGamRob <- function(
		data,
		variable,
		omit.missing = T,
		dist.family,
		estimator = "tdmean",
		method = "MLE + Robust",
		print.short.p = F,
		print.long.p = F,
		plotOverD.p = F,
		plotQuantile.p = F,
		control.float.1 = 0.5,
		control.float.2 = 20.5,
		control.float.3 = 0.99,
		control.float.4 = 0.4,
		control.float.5 = 0.4,
		control.float.6 = 0.0001,
		control.integer.1 = 101,
		control.integer.2 = 1,
		control.integer.3 = 1,
		control.integer.4 = 100,
		control.logical.1 = T,
		control.string.1 = "c(0,0,0)",
		MLE.maxit = 100,
		MLE.tol = 0.001)
{
	MLE.control.call <- call("gammaMLE.control")
	if(MLE.maxit != 100)
		MLE.control.call$maxit <- MLE.maxit
	if(MLE.tol != 1e-3)
		MLE.control.call$tol <- MLE.tol

	fun.call <- match.call()
	dist.family <- casefold(dist.family)

	if(dist.family == "lognormal")
		dist.family <- "lognorm"

	if(!missing(variable) && nchar(variable))
		data <- substitute(data[[variable]])
	else
		data <- substitute(data)

	switch(dist.family,

		gamma = {
			if(estimator == "tdmean") {
				robust.control.call <- call("gammaRob.control", estim = "tdmean")

				if(control.float.1 != 0.5)
					robust.control.call$alpha1 <- control.float.1

				if(control.float.2 != 20.5)
					robust.control.call$alpha2 <- control.float.2

				if(control.float.3 != 0.99)
					robust.control.call$u <- control.float.3

				if(control.float.4 != 0.4)
					robust.control.call$beta <- control.float.4

				if(control.float.5 != 0.4)
					robust.control.call$gam <- control.float.5

				if(control.float.6 != 0.0001)
					robust.control.call$tol <- control.float.6

				if(control.logical.1 != T)
					robust.control.call$cov <- control.logical.1
			}

			else {
				robust.control.call <- call("gammaRob.control", estim = "M")

				if(control.float.1 != 1.5)
					robust.control.call$b1 <- control.float.1

				if(control.float.2 != 1.7)
					robust.control.call$b2 <- control.float.2

				if(control.float.3 != 0.0001)
					robust.control.call$tol <- control.float.3

				if(control.float.4 != 0.001)
					robust.control.call$til <- control.float.4

				if(control.float.5 != 0)
					robust.control.call$sigma <- control.float.5

				if(control.logical.1 != T)
					robust.control.call$cov <- control.logical.1

				if(control.integer.1 != 101)
					robust.control.call$k <- control.integer.1

				if(control.integer.2 != 1)
					robust.control.call$maxta <- control.integer.2

				if(control.integer.3 != 1)
					robust.control.call$maxtc <- control.integer.3
				
				if(control.integer.4 != 100)
					robust.control.call$maxit <- control.integer.4

				if(	control.string.1[1] != 0 ||
						control.string.1[2] != 0 ||
						control.string.1[3] != 0)
					robust.control.call$A <- control.string.1
			}
		}
		,
		weibull = {
			if(estimator == "tdmean") {
				robust.control.call <- call("weibullRob.control", estim = "tdmean")

				if(control.float.1 != 0.5)
					robust.control.call$alpha1 <- control.float.1

				if(control.float.2 != 20.5)
					robust.control.call$alpha2 <- control.float.2

				if(control.float.3 != 0.99)
					robust.control.call$u <- control.float.3

				if(control.float.4 != 0.4)
					robust.control.call$beta <- control.float.4

				if(control.float.5 != 0.4)
					robust.control.call$gam <- control.float.5

				if(control.float.6 != 0.0001)
					robust.control.call$tol <- control.float.6

				if(control.logical.1 != T)
					robust.control.call$cov <- control.logical.1
			}

			else {
				robust.control.call <- call("weibullRob.control", estim = "M")

				if(control.float.1 != 1.5)
					robust.control.call$b1 <- control.float.1

				if(control.float.2 != 1.7)
					robust.control.call$b2 <- control.float.2

				if(control.float.3 != 0.0001)
					robust.control.call$tol <- control.float.3

				if(control.float.4 != 0.001)
					robust.control.call$til <- control.float.4

				if(control.float.5 != 0)
					robust.control.call$sigma <- control.float.5

				if(control.logical.1 != T)
					robust.control.call$cov <- control.logical.1
				
				if(control.integer.4 != 100)
					robust.control.call$maxit <- control.integer.4

				if(	control.string.1[1] != 0 ||
						control.string.1[2] != 0 ||
						control.string.1[3] != 0)
					robust.control.call$A <- control.string.1
			}
		}
		,
		lognorm = {
			robust.control.call <- call("lognormRob.control", estim = "tdmean")

			if(control.float.1 != 0.5)
				robust.control.call$alpha1 <- control.float.1

			if(control.float.2 != 20.5)
				robust.control.call$alpha2 <- control.float.2

			if(control.float.3 != 0.99)
				robust.control.call$u <- control.float.3

			if(control.float.4 != 0.4)
				robust.control.call$beta <- control.float.4

			if(control.float.5 != 0.4)
				robust.control.call$gam <- control.float.5

			if(control.float.6 != 0.0001)
				robust.control.call$tol <- control.float.6

			if(control.logical.1 != T)
				robust.control.call$cov <- control.logical.1
		}
	)

	model.list <- list()

	if(method != "MLE") {

		robust.call <- call(paste(dist.family, "Rob", sep = ""),
											data = data)

		if(estimator != "tdmean")
			robust.call$estim <- substitute(estimator)

		if(length(robust.control.call) > 2)
			robust.call$control <- substitute(robust.control.call)

		model.list$Robust <- substitute(robust.call)
	}


	if(method != "Robust") {

		mle.call <- call(paste(dist.family, "MLE", sep = ""),
									data = data)

		if(length(MLE.control.call) > 1)
			mle.call$control <- substitute(MLE.control.call)

		model.list$MLE <- substitute(mle.call)
	}

	tmp <- fit.models(model.list = model.list)

	if(print.short.p) print(tmp)
	if(print.long.p) print(summary(tmp))

	if(any(plotOverD.p, plotQuantile.p)) {
		whichPlots <- (2:3)[c(plotOverD.p, plotQuantile.p)]
		plot(tmp, which = whichPlots)
	}

	invisible(tmp)
}







