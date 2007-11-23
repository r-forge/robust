##
##	menuGlmRob
##

menuGlmRob <- function(
	formula,
	family,
	link = identity,
	variance,
	data,
	subset,
	na.omit.p = T,
	epsilon.mle = 0.0001,
	maxit.mle = 10,
	trace.mle = F, 
	print.short.p = F,
	print.long.p = T,
	print.anova.p = T, 
	print.correlation.p = F,
	save.name = NULL,
	save.fit.p = F,
	save.resid.working.p = F,
	save.resid.pearson.p = F,
	save.resid.deviance.p = F,
	save.resid.response.p = F,
	plotResidVsFit.p = F,
	plotResponseVsFit.p = F,
	plotResidvsIndex.p = f,
	smooths.p = F,
	rugplot.p = F,
	id.n = 3,
	plotPartialResid.p = F,
	plotPartialFit.p = F,
	rugplotPartialResid.p = F,
	scalePartialResid.p = T,
	newdata = NULL,
	predobj.name = NULL,
	predict.type = "link",
	predict.p = F,
	se.p = F,
	fitting.method = "MLE + Robust",
	compare.glmfits.rrvsrd = F,
	pearson.qq.p = F,
	deviance.qq.p = F,
	glmrob.fit.method = 'cubif',
	wt.fn = "carroll",
	wt.tuning = 8,
	mc.gamma = 0.01,
	mc.maxit = 30,
	mc.trc = F,
	mc.tol = 0.001,
	mc.initial = NULL,
	epsilon = 0.001,
	maxit = 50,
	bpar = 2,
	cpar = 1.5,
	trc = F) 
{
	control.call <- call("glm.control")

	if(epsilon.mle != 0.0001)
			control.call$epsilon <- epsilon.mle
	if(maxit.mle != 10)
			control.call$maxit <- maxit.mle
	if(trace.mle)
			control.call$trace <- trace.mle

	cubif.control.call <- call("glmRob.cubif.control")

	if(maxit !=50 )
	  cubif.control.call$maxit<- maxit
	if(epsilon != 0.001)
	  cubif.control.call$epsilon <- epsilon
	if(trc)
	  cubif.control.call$trace <- trc
	if(cpar != 1.5)
	  cubif.control.call$cpar <- cpar
	if(bpar != 2)
	  cubif.control.call$bpar <- bpar
	
	misclass.control.call <- call("glmRob.misclass.control")

	if(mc.gamma != 0.01)
		misclass.control.call$mc.gamma <- mc.gamma
	if(mc.maxit != 30)
		misclass.control.call$mc.maxit <- mc.maxit
	if(mc.tol != 0.001 )
		misclass.control.call$mc.tol <- mc.tol
	if(mc.trc)
		misclass.control.call$mc.trc <- mc.trc
	if(!is.null(mc.initial))
		misclass.control.call$mc.initial <- mc.initial
	
	mallows.control.call <- call("glmRob.mallows.control")

	if(wt.fn != "carroll")
		mallows.control.call$mallows.fn <- as.name("wt.huber")
	if(wt.tuning != 8)
		mallows.control.call$mallows.tuning <- wt.tuning

	fun.call <- match.call()

	if(na.omit.p)
		fun.call$na.action <- as.name("na.exclude")
	else
		fun.call$na.action <- as.name("na.fail")

	if(fitting.method == "MLE" || fitting.method == "MLE + Robust")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(glm))
		mle.call <- fun.call[c(T, fun.args)]
		mle.call[[1]] <- as.name("glm")
		if(length(control.call) > 1)
			mle.call$control <- substitute(control.call)
	}
	
	if(fitting.method == "Robust" || fitting.method == "MLE + Robust")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(glmRob))
		robust.call <- fun.call[c(T, fun.args)]
		robust.call[[1]] <- as.name("glmRob")
		
		if(length(cubif.control.call) > 1 && glmrob.fit.method == "cubif")
			robust.call$cubif.control <- substitute(cubif.control.call)

		if(glmrob.fit.method == "misclass")
		{
			robust.call$fit.method <- "misclass"
			if(length(misclass.control.call) > 1)
				robust.call$misclass.control <- substitute(misclass.control.call)			
		}

		if(glmrob.fit.method == "mallows")
		{
			robust.call$fit.method <- "mallows"
			if(length(mallows.control.call) > 1)
				robust.call$mallows.control <- substitute(mallows.control.call)
		}
	}
	
	if(fitting.method == "MLE + Robust")
		fun.call <- call("fit.models", model.list = list(Robust =
			substitute(robust.call), MLE = substitute(mle.call)))
	else if(fitting.method == "MLE")
		fun.call <- call("fit.models", model.list = list(MLE =
			substitute(mle.call)))
	else
		fun.call <- call("fit.models", model.list = list(Robust =
			substitute(robust.call)))

	retobj <- eval(fun.call)



##  Summary function:
		if(print.short.p || print.long.p || print.anova.p) {

			if(length(retobj) == 1)
				cat(paste("\n\t*** GLM: ", names(retobj), " Fit ***\n", sep = ""))
			else	
				cat("\n\t*** GLM Fits Comparison ***\n")

			if(print.short.p) {
				print.fit.models(retobj)
			}

			if(print.long.p) {
				print(summary.glmfm(retobj, correlation = print.correlation.p))
			}

			if(print.anova.p) {
				for(i in 1:length(retobj))
				{
					cat(paste("\n", names(retobj)[[i]], ": ", sep = ""))
					print(anova(retobj[[i]]))
				}
			}
			cat("\n")
		}

## Plot function

		if(any(c(plotResidVsFit.p, plotResponseVsFit.p, pearson.qq.p,
			deviance.qq.p, compare.glmfits.rrvsrd, plotResidvsIndex.p)))
		{
			whichPlots <- (2:7)[c(plotResidVsFit.p, plotResponseVsFit.p,
				pearson.qq.p, deviance.qq.p, compare.glmfits.rrvsrd,
				plotResidvsIndex.p)]

			plot.glmfm(retobj, which.plots = whichPlots,
				chisq.percent = 0.99, vertical.outlier = 0.99,
				smooths = smooths.p, rugplot = rugplot.p, id.n = id.n)
		}

		if(plotPartialResid.p)
		{
			partial.plot.lmfm(retobj, fit = plotPartialFit.p,
				rugplot = rugplotPartialResid.p, scale = scalePartialResid.p)
		}

	##
	## Save Results
	##

	if(any(c(save.fit.p, save.resid.working.p, save.resid.pearson.p,
			save.resid.deviance.p, save.resid.response.p)))
		{
		saveobj <- list()
		if(class(retobj) != "fit.models")
			{
				if(save.fit.p)
					saveobj$fitted.values <- fitted(retobj[[1]])
				if(save.resid.working.p)
					saveobj$working.residuals <- residuals(retobj[[1]], type = 'working')
				if(save.resid.pearson.p)
					saveobj$pearson.residuals <- residuals(retobj[[1]], type = 'pearson')
				if(save.resid.deviance.p)
					saveobj$deviance.residuals <- residuals(retobj[[1]], type = 'deviance')
				if(save.resid.response.p)
					saveobj$response.residuals <- residuals(retobj[[1]], type = 'response')
			}
		else
			{
			if(save.fit.p)
				{
					saveobj$Robust$fitted <- fitted(retobj[[1]])
					saveobj$MLE$fitted <- fitted(retobj[[2]])
				}
			if(save.resid.working.p)
				{
					saveobj$Robust$working.residuals <- residuals(retobj[[1]], type = 'working')
					saveobj$MLE$working.residuals <- residuals(retobj[[2]], type = 'working')
				}
			if(save.resid.pearson.p)
				{
					saveobj$Robust$pearson.residuals <- residuals(retobj[[1]], type = 'pearson')
					saveobj$MLE$pearson.residuals <- residuals(retobj[[2]], type = 'pearson')
				}
			if(save.resid.deviance.p)
				{
					saveobj$Robust$deviance.residuals <- residuals(retobj[[1]], type = 'deviance')
					saveobj$MLE$deviance.residuals <- residuals(retobj[[2]], type = 'deviance')
				}
			if(save.resid.response.p)
				{
				saveobj$Robust$response.residuals <- residuals(retobj[[1]], type = 'response')
				saveobj$MLE$response.residuals <- residuals(retobj[[2]], type = 'response')
				}
			}
 		saveobj <- guiAsDefaultDataObject(saveobj)
		spropSaveResultColumns(saveobj, save.name, .Options$show.data.in.view)
	}

	if(predict.p || se.p)
		tabPredict.glmRob(retobj, newdata = newdata,
		save.name = predobj.name, predict.type = predict.type,
		predict.p = predict.p, se.p = se.p)

	invisible(retobj)
}

