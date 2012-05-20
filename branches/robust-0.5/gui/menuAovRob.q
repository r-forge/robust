"menuAovRob" <- function(
	formula,
	data,
	subset=NULL,
	weights=NULL, 
	na.omit.p=F,
	rob.rad.p = "LS + Robust",
	contrasts=NULL,
	print.object.p=F,
	sequentialSS.p=T,
	partialSS.p=F,
	coef.p=F,
	dummy.coef.p = F,
	means.p = F,
	lsmeans.p = F,
	save.results=NULL,
	save.fitted.p=F,
	save.resid.p=F,
	plotResidVsFit.p=F, 
	plotResponseVsFit.p=F,
	plotQQ.p=F,
	plotResidualDensity.p=F,
	plotResidualsVsIndex.p=F,
	overlaidQ.p=F,
	overlaidD.p=F,
	smooths.p=F,
	rugplot.p=F,
	QQenvelope.p=T,
	robQQline.p=T,
	halfNormal.p=F,
	id.n=3,
	plotPartialResid.p=F,
	plotPartialFit.p = F, 
	rugplotPartialResid.p=F,
	scalePartialResid.p=T,
	second.p = "MM",
	weight2.p = "Optimal",
	eff.p = 0.9,
	mxicoef.p = 50,
	mxiscale.p = 50,
	mxiref.p = 50,
	tolconvrg.p = 0.0001,
	tolscale.p = 1e-6,
	tolrank.p = 1.5e-6)
	{
## Construct Control Lists

	control.call <- call("lmRob.robust.control")
	if (second.p != "MM")
		control.call$final.alg <- second.p
	if (weight2.p != "Optimal")
		control.call$weight <- c(weight2.p, weight2.p)
	if (eff.p != 0.9)
		control.call$efficiency <- eff.p
	if (mxicoef.p != 50)
		control.call$mxf <- mxicoef.p
	if (mxiscale.p != 50)
		control.call$mxs <- mxiscale.p
	if (mxiref.p != 50)
		control.call$mxr <- mxiref.p
	if (tolconvrg.p != 0.0001)
		control.call$tlo <- tolconvrg.p
	if (tolscale.p != 1e-6)
		control.call$tl  <- tolscale.p
	if (tolrank.p != 1.5e-6)
		control.call$tua <- tolrank.p

## Model page

	data.name <- deparse(substitute(data))
	
##
##	construct call
##

	fun.call <- match.call()

	if (rob.rad.p == "LS")
	{
		fun.call[[1]] <- as.name("aov")
		fun.args <- is.element(arg.names(fun.call), arg.names(aov))
		header.txt <- paste("\n\t***  Classical ANOVA for data in: ",
			data.name, "  ***\n\n")
	}
	
	else if (rob.rad.p == "Robust")
	{
		fun.call[[1]] <- as.name("aovRob")
		fun.args <- is.element(arg.names(fun.call), arg.names(aovRob))
		header.txt <- paste("\n\t***  Robust ANOVA for data in: ",
			data.name, "  ***\n\n")
	}

	else
	{
		fun.call[[1]] <- as.name("fit.models")
		fun.args <- is.element(arg.names(fun.call), arg.names(aovRob))
		header.txt <- paste("\n\t***  Robust and Classical AONVA comparison for data in: ",
			data.name, "  ***\n\n")
	}

	if(!is.null(contrasts))
		fun.call$contrasts <- substitute(contrasts)

	extra.args <- list()
	if(na.omit.p)
		extra.args$na.action <- as.name("na.exclude")
	if(!is.null(subset))
		extra.args$subset <- substitute(subset)
	if(!is.null(weights))
		extra.args$weights <- substitute(weights)

	fun.call <- fun.call[c(T, fun.args)]
	if(length(extra.args))
		fun.call <- as.call(c(fun.call, extra.args))
	if(rob.rad.p == "Robust" && length(control.call) > 1)
		fun.call$robust.control <- control.call

	if(rob.rad.p == "LS + Robust")
	{
		both.call <- call("fit.models")
		both.call$model.list <- list(Robust = fun.call, LS = fun.call)
		both.call[[2]][[1]][[1]] <- as.name("aovRob")
		both.call[[2]][[2]][[1]] <- as.name("aov")
		if(length(control.call) > 1)
			both.call[[2]][[1]]$robust.control <- control.call
		fun.call <- both.call
	}
	else if (rob.rad.p == "Robust")
		fun.call <- call("fit.models", model.list = list(Robust = fun.call))

	else
		fun.call <- call("fit.models", model.list = list(LS = fun.call))

	aov.obj <- eval(fun.call, sys.parent() )

		
## Results page
	if(any(c(print.object.p, sequentialSS.p, partialSS.p, coef.p,
		dummy.coef.p, save.resid.p, save.fitted.p)))
		tabAnova.aovRob(object=aov.obj,
			header = header.txt,
			print.object.p=print.object.p, 
			sequentialSS.p=sequentialSS.p,
			partialSS.p = partialSS.p,
			coef.p=coef.p,
			dummy.coef.p = dummy.coef.p,
			save.results=save.results,
			save.resid.p=save.resid.p, 
			save.fitted.p=save.fitted.p)

	if(means.p) {
		if(!is.null(aov.obj$LS)) {
			if(is.null(aov.obj$LS$qr))
				aov.obj$LS <- update(aov.obj$LS, qr = T)
			print(model.tables(aov.obj$LS, type = "means"))
		}
		else {
			LS <- call("aov", formula = substitute(formula),
							data = substitute(data), qr = T)
			LS <- eval(LS)
			print(model.tables(LS, type = "means"))
		}
	}

# Call plot function:
	if(any(c(plotResidVsFit.p,
		plotResponseVsFit.p, 
		plotQQ.p,
		plotResidualDensity.p,
		plotResidualsVsIndex.p,
		overlaidQ.p,
		overlaidD.p)))
			{
			whichPlots <- (c(4,6,2,3,8,9,10))[c(plotResidVsFit.p,
				plotResponseVsFit.p, plotQQ.p, plotResidualDensity.p,
				plotResidualsVsIndex.p, overlaidQ.p, overlaidD.p)]

			plot.fit.models(aov.obj, which.plots=whichPlots,
				smooths = smooths.p, rugplot = rugplot.p,
				envelope = QQenvelope.p, robustQQline = robQQline.p,
				half.normal = halfNormal.p, id.n = id.n)
			}

	if(plotPartialResid.p)
		partial.plot.lmfm(aov.obj, fit = plotPartialFit.p,
			scale = scalePartialResid.p, rugplot = rugplotPartialResid.p)

	invisible(aov.obj)
}



"tabAnova.aovRob" <- function(object, header, print.object.p=T,
	sequentialSS.p = T, partialSS.p = F, coef.p = F, dummy.coef.p = F, 
	save.results = NULL, save.resid.p= F,
	save.fitted.p = F, show.p = .Options$show.data.in.view)
{
	if(print.object.p || coef.p || sequentialSS.p) {
		cat(header)
	}
	if(print.object.p) {
		cat("\nShort Output:\n")
		print(object)
	}
	if(sequentialSS.p) {
		cat("\n")
		print(summary(object))
	}
	if(partialSS.p) {
		cat("\nComparison of Type III Sums of Squares:\n")
		print(type3.fm(object))
	}
	if(coef.p) {
		cat("\nEstimated Coefficients:\n")
		print(coef(object))
	}
	if(dummy.coef.p) {
		cat("\nEstimated K Coefficients for K-level Factor:\n")
		print(dummy.coef(object))
	}
	
### Save results if requested: ###

	if(any(c(save.fitted.p, save.resid.p))) {
		saveobj <- list()
		if(class(object) != "fit.models")
		{
			if(save.fitted.p)
				saveobj[["fit"]] <- fitted(object)
			if(save.resid.p)
				saveobj[["residuals"]] <- residuals(object)
		}
		else
		{
			if(save.fitted.p) {
				saveobj$Robust$fitted <- fitted(object[[1]])
				saveobj$LS$fitted <- fitted(object[[2]])
			}
			if(save.resid.p) {
				saveobj$Robust$residuals <- residuals(object[[1]])
				saveobj$LS$residuals <- residuals(object[[2]])
			}
		}
 			saveobj <- guiAsDefaultDataObject(saveobj)
			spropSaveResultColumns(saveobj, save.results, show.p)
     }
	invisible(object)
}
