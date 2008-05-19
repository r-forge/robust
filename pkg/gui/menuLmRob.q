menuLmRob <- function(
		formula,
		data,
		weights,
		subset,
		na.omit.p = T,
		fitting.p = "LS + Robust",
		model.selection = F,
		print.short.p = T,
		print.long.p = F,
		print.correlation.p = F,
		print.anova.p = T,
		bootstrap.se = F,
		save.name = NULL,
		save.fit.p = F,
		save.resid.p = F, 
		plotResidVsFit.p = F,
		plotResponseVsFit.p = F,
		plotQQ.p = F,
		plotRRRD.p = F,
		plotRDensity.p = F, 
		plotTime.p = F,
		plotOneVariable.p = F,
		plotOverQ.p = F,
		plotOverD.p=F,
		smooths.p = F,
		rugplot.p = F,
		QQplotEnvelope.p = F,
		robQQline.p = F,
		halfNormal.p = F,
		id.n = 3,
		plotPartial.p = F,
		plotPartialFit.p = F,
		plotRugplot.p = F,
		plotScale.p=F,
		newdata = NULL, 
		predobj.name = NULL,
		predict.p = F,
		ci.p = F,
		se.p = F,
		conf.level = 0.95, 
		final.alg = "MM",
		weight = "Optimal",
		efficiency = 0.9,
		mxf = 50,
		mxr = 50,
		mxs = 50,
		tlo = 0.0001,
		tl = 1e-6,
		tau = 1.5e-6,
		initial.alg = "Auto",
		nrep.p = "Auto",
		seed = 1313,
		popsize = "Auto",
		births.n = "Auto",
		maxobs = "Auto",
		mutate.prob = "Auto",
		stock = "Auto",
		stockprob = "Auto")
{
	fun.call <- match.call()

	#generate control calls
	
	robust.control.call <- call("lmRob.robust.control")
	if (efficiency != 0.9)
		robust.control.call$efficiency <- efficiency
	if (mxf != 50)
		robust.control.call$mxf <- mxf
	if (mxr != 50)
		robust.control.call$mxr <- mxr
	if (mxs != 50)
		robust.control.call$mxs <- mxs
	if (tlo != 0.0001)
		robust.control.call$tlo <- tlo
	if (tl != 1e-6)
		robust.control.call$tl  <- tl
	if (tau != 1.5e-6)
		robust.control.call$tau <- tau
	if (seed != 1313)
		robust.control.call$seed <- seed
	if (initial.alg != "Auto")
		robust.control.call$initial.alg <- initial.alg
	if (final.alg != "MM")
		robust.control.call$final.alg <- final.alg
	if (weight != "Optimal")
		robust.control.call$weight <- c(weight, weight)

	if (initial.alg == "Genetic") {
		genetic.control.call <- call("lmRob.genetic.control")
		if (popsize != "Auto")
			genetic.control.call$popsize <- as.integer(popsize)
		if (births.n != "Auto")
			genetic.contro.calll$births.n <- as.integer(births.n)
		if (maxobs != "Auto")
			genetic.control.call$maxslen <- as.integer(maxobs)
		if (mutate.prob != "Auto")
			genetic.control.call$mutate.prob <- mutate.prob
		if (stock != "Auto")
			genetic.control.call$stock <- stock
		if (stockprob != "Auto")
			genetic.control.call$stockprob <- stockprob
		}

	if(na.omit.p)
		fun.call$na.action <- as.name("na.exclude")
	else
		fun.call$na.action <- NULL

	if(fitting.p == "LS" | fitting.p == "LS + Robust")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(lm))
		ls.call <- fun.call[c(T, fun.args)]
		ls.call[[1]] <- as.name("lm")
	}
	
	if(fitting.p == "Robust" | fitting.p == "LS + Robust")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(lmRob))
		robust.call <- fun.call[c(T, fun.args)]
		robust.call[[1]] <- as.name("lmRob")
		if(length(robust.control.call) > 1)
			robust.call$robust.control <- substitute(robust.control.call)
		if(initial.alg == "Genetic" && length(genetic.control.call) > 1)
			robust.call$genetic.control <- substitute(genetic.control.call)
	}
	
	if(fitting.p == "LS + Robust") {
		fun.call <- call("fit.models",
			model.list = list(Robust = substitute(robust.call),
			LS = substitute(ls.call)))
		retobj <- eval(fun.call)
	}
	
	else if(fitting.p == "LS") {
		fun.call <- call("fit.models", model.list = list(LS = substitute(ls.call)))
		retobj <- eval(fun.call)

		if(model.selection) {
			retobj <- retobj$LS
			cat("\n\t*** Model Selection ***\n\n")
			retobj <- step(retobj, trace = F)
			retobj <- list(LS = retobj)
			attr(retobj, "model.list") <- list(LS = retobj$LS$call)
			attr(retobj, "virtual.class") <- "lmfm"
			oldClass(retobj) <- "fit.models"
		}
	}

	else {
		fun.call <- call("fit.models", model.list = list(Robust = substitute(robust.call)))
		retobj <- eval(fun.call)

		if(model.selection) {
			retobj <- retobj$Robust
			cat("\n\t*** Model Selection ***\n\n")
			retobj <- step(retobj, trace = F)
			retobj <- list(Robust = retobj)
			attr(retobj, "model.list") <- list(Robust = retobj$Robust$call)
			attr(retobj, "virtual.class") <- "lmfm"
			oldClass(retobj) <- "fit.models"
		}
	}
	
#------------------------
# Call summary function
#------------------------

	if (fitting.p == "LS")
		title <- "\n\t*** Linear Regression ***\n"
	else if (fitting.p == "Robust") 
		title <- "\n\t*** Robust Linear Regression ***\n"
	else
		title <- "\n\t*** Classical and Robust Linear Regression ***\n"

	if(model.selection)
		print(retobj[[1]]$anova)

	if(print.short.p) {
		cat(title)
		print(retobj)
	}

	if(print.long.p) {
		cat(title)

		if(bootstrap.se && fitting.p == "Robust")
			print(summary(retobj[[1]], correlation = print.correlation.p,
						bootstrap.se = bootstrap.se))
		else
			print(summary(retobj, correlation = print.correlation.p))
	}

	if(print.anova.p) {
		cat("\n")
		for(i in 1:length(retobj)) {
			cat(paste("For", names(retobj)[i], "fit:\n"))
			print(anova(retobj[[i]]))
			cat("\n")
		}
	}
			
#---------------------
# Call plot function
#---------------------

	if(any(c(plotResidVsFit.p, plotResponseVsFit.p, plotQQ.p, plotOneVariable.p, 
		plotTime.p, plotRDensity.p, plotRRRD.p, plotOverQ.p, plotOverD.p))) 
	{
		whichPlots <- c(2:5,7:11)[c(plotQQ.p, plotRDensity.p, plotRRRD.p,
			plotResidVsFit.p, plotResponseVsFit.p, plotTime.p,
			plotOverQ.p, plotOverD.p, plotOneVariable.p)]
		if (length(whichPlots))
			plot(retobj, which.plots = whichPlots, smooths = smooths.p,
				rugplot = rugplot.p, id.n = id.n, envelope = QQplotEnvelope.p,
				half.normal = halfNormal.p, robustQQline = robQQline.p)
	}

	if(model.selection) {
		step.plot <- function(object) {
			x <- object$anova
			x.names <- as.character(x[[1]])
			x.names[1] <- "Full"
			y <- x[[4]]
			plot(1:length(y), y, axes = F, xlab = "model", ylab = "RFPE",
				type = "b", pch = 16, main = "Model Selection")
			box()
			axis(2)
			axis(1, at = 1:length(y), labels = x.names, crt = 30)
			invisible()
		}
		
		step.plot(retobj[[1]])
	}

	if(plotPartial.p)
		partial.plot.lmfm(retobj, fit = plotPartialFit.p,
			scale = plotScale.p, rugplot = plotRugplot.p)

#---------------#
# Call predict: #
#---------------#

	if(any(c(predict.p, ci.p, se.p)))
		tabPredict.lmRob(retobj, newdata, predobj.name, predict.p, 
			ci.p, se.p, conf.level)

### Save results if requested: ###

	if(any(c(save.fit.p, save.resid.p))) {
		saveobj <- list()
		if(class(retobj) != "fit.models")
		{
			if(save.fit.p)
				saveobj[["fit"]] <- fitted(retobj)
			if(save.resid.p)
				saveobj[["residuals"]] <- residuals(retobj)
		}
		else
		{
			if(save.fit.p) {
				saveobj$Robust$fitted <- fitted(retobj[[1]])
				saveobj$LS$fitted <- fitted(retobj[[2]])
			}
			if(save.resid.p) {
				saveobj$Robust$residuals <- residuals(retobj[[1]])
				saveobj$LS$residuals <- residuals(retobj[[2]])
			}
		}
 			saveobj <- guiAsDefaultDataObject(saveobj)
			spropSaveResultColumns(saveobj, save.name,
				.Options$show.data.in.view)
     }

	invisible(retobj)

}









