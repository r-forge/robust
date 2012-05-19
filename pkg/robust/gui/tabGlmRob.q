##
## file: tabglmRob.q
##

	tabPredict.glmRob <- function(object, newdata = NULL,
		save.name, predict.type, predict.p = F, se.p = F,
		show.p = .Options$show.data.in.view)
	{

		if(class(object) == "fit.models") {
			if(!is.null(object$Robust))
				tabPredict.glmRob(object$Robust, newdata = newdata,
					save.name = save.name, predict.type = predict.type,
					predict.p = predict.p, se.p = se.p, show.p = show.p)
			if(!is.null(object$MLE))
				tabPredict.glmRob(object$MLE, newdata = newdata,
					save.name = save.name, predict.type = predict.type,
					predict.p = predict.p, se.p = se.p, show.p = show.p)
			return(invisible())
		}

		else {
			if(is.null(newdata))
				predobj <- predict(object, type = predict.type, 
					se.fit = se.p)
			else
				predobj <- predict(object, newdata, 
					type = predict.type, se.fit = se.p)
			if(!predict.p)
				predobj$fit <- NULL
			if(!se.p)
				predobj$se.fit <- NULL
			predobj$residual.scale <- NULL
			predobj$df <- NULL
			predobj <- guiAsDefaultDataObject(predobj)
			if(class(object) == "glm")
				names(predobj) <- paste("MLE", names(predobj),
					sep = ".")
			else
				names(predobj) <- paste("Robust", names(predobj),
					sep = ".")
			spropSaveResultColumns(predobj, save.name, show.p)
			invisible()
		}
	}

