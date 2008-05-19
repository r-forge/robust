tabPredict.lmRob <- function(object, newdata = NULL, save.name,
	predict.p = F, ci.p = F, se.p = F, conf.level = 0.95,
	show.p = .Options$show.data.in.view)

{
	if(class(object) == "fit.models") {

		if(!is.null(object$Robust))
			tabPredict.lmRob(object$Robust, newdata = newdata,
				save.name = save.name, predict.p = predict.p,
				ci.p = ci.p, se.p = se.p, conf.level = conf.level,
				show.p = show.p)
			
		if(!is.null(object$LS))
			tabPredict.lmRob(object$LS, newdata = newdata,
				save.name = save.name, predict.p = predict.p,
				ci.p = ci.p, se.p = se.p, conf.level = conf.level,
				show.p = show.p)

		return(invisible())
	}

	if(inherits(object, "lmRob"))
		pred.func <- predict.lmRob
	else 
		pred.func <- predict.gam
	if(is.null(newdata))
		predobj <- pred.func(object, type = "response", se.fit = se.p || 
			ci.p)
	else 
		predobj <- pred.func(object, newdata, type = "response", se.fit = 
			se.p || ci.p)

	if(ci.p) {
		if(conf.level > 1 && conf.level < 100)
			conf.level <- conf.level/100
		t.value <- qt(conf.level, object$df.residual)
		lower.name <- paste("LCL", conf.level * 100, sep = "")
		upper.name <- paste("UCL", conf.level * 100, sep = "")
		predobj[[lower.name]] <- predobj$fit - t.value * predobj$se.fit
		predobj[[upper.name]] <- predobj$fit + t.value * predobj$se.fit
	}

# remove prediction column and se column if not requested:

	if(!predict.p)
		predobj$fit <- NULL
	if(!se.p)
		predobj$se.fit <- NULL
	predobj$residual.scale <- NULL
	predobj$df <- NULL
	predobj <- guiAsDefaultDataObject(predobj)
	if(ncol(predobj) == 1)
		names(predobj) <- "fit"
	if(class(object) == "lm")
		names(predobj) <- paste("LS", names(predobj), sep = ".")
	else
		names(predobj) <- paste("Robust", names(predobj), sep = ".")
	spropSaveResultColumns(predobj, save.name, show.p)
	invisible(NULL)
}
