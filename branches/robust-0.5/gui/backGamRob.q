backGamRob <- function(data)
{
	setAdvanced <- function(data) {
		switch(cbGetCurrValue(data, "gamRobDistn"),

			Gamma = {
				if(cbGetCurrValue(data, "gamRobEstim") == "tdmean") {
					data <- cbSetPrompt(data, "gamRobFloat1", "alpha 1:")
					data <- cbSetPrompt(data, "gamRobFloat2", "alpha 2:")
					data <- cbSetPrompt(data, "gamRobFloat3", "u:")
					data <- cbSetPrompt(data, "gamRobFloat4", "beta:")
					data <- cbSetPrompt(data, "gamRobFloat5", "gam:")
					data <- cbSetPrompt(data, "gamRobFloat6", "tol:")

					data <- cbSetCurrValue(data, "gamRobFloat1", 0.5)
					data <- cbSetCurrValue(data, "gamRobFloat2", 20.5)
					data <- cbSetCurrValue(data, "gamRobFloat3", 0.99)
					data <- cbSetCurrValue(data, "gamRobFloat4", 0.4)
					data <- cbSetCurrValue(data, "gamRobFloat5", 0.4)
					data <- cbSetCurrValue(data, "gamRobFloat6", 0.0001)

					data <- cbSetEnableFlag(data, "gamRobFloat1", T)
					data <- cbSetEnableFlag(data, "gamRobFloat2", T)
					data <- cbSetEnableFlag(data, "gamRobFloat3", T)
					data <- cbSetEnableFlag(data, "gamRobFloat4", T)
					data <- cbSetEnableFlag(data, "gamRobFloat5", T)
					data <- cbSetEnableFlag(data, "gamRobFloat6", T)
					data <- cbSetEnableFlag(data, "gamRobInteger1", F)
					data <- cbSetEnableFlag(data, "gamRobInteger2", F)
					data <- cbSetEnableFlag(data, "gamRobInteger3", F)
					data <- cbSetEnableFlag(data, "gamRobInteger4", F)
					data <- cbSetEnableFlag(data, "gamRobString", F)
				}

				else {
					data <- cbSetPrompt(data, "gamRobFloat1", "b1:")
					data <- cbSetPrompt(data, "gamRobFloat2", "b2:")
					data <- cbSetPrompt(data, "gamRobFloat3", "tol:")
					data <- cbSetPrompt(data, "gamRobFloat4", "til:")
					data <- cbSetPrompt(data, "gamRobFloat5", "sigma:")

					data <- cbSetCurrValue(data, "gamRobFloat1", 1.5)
					data <- cbSetCurrValue(data, "gamRobFloat2", 1.7)
					data <- cbSetCurrValue(data, "gamRobFloat3", 0.0001)
					data <- cbSetCurrValue(data, "gamRobFloat4", 0.001)
					data <- cbSetCurrValue(data, "gamRobFloat5", 0)

					data <- cbSetEnableFlag(data, "gamRobFloat1", T)
					data <- cbSetEnableFlag(data, "gamRobFloat2", T)
					data <- cbSetEnableFlag(data, "gamRobFloat3", T)
					data <- cbSetEnableFlag(data, "gamRobFloat4", T)
					data <- cbSetEnableFlag(data, "gamRobFloat5", T)
					data <- cbSetEnableFlag(data, "gamRobFloat6", F)
					data <- cbSetEnableFlag(data, "gamRobInteger1", T)
					data <- cbSetEnableFlag(data, "gamRobInteger2", T)
					data <- cbSetEnableFlag(data, "gamRobInteger3", T)
					data <- cbSetEnableFlag(data, "gamRobInteger4", T)
					data <- cbSetEnableFlag(data, "gamRobString", T)
				}
			}
			,
			Weibull = {
				if(cbGetCurrValue(data, "gamRobEstim") == "tdmean") {
					data <- cbSetPrompt(data, "gamRobFloat1", "alpha 1:")
					data <- cbSetPrompt(data, "gamRobFloat2", "alpha 2:")
					data <- cbSetPrompt(data, "gamRobFloat3", "u:")
					data <- cbSetPrompt(data, "gamRobFloat4", "beta:")
					data <- cbSetPrompt(data, "gamRobFloat5", "gam:")
					data <- cbSetPrompt(data, "gamRobFloat6", "tol:")

					data <- cbSetCurrValue(data, "gamRobFloat1", 0.5)
					data <- cbSetCurrValue(data, "gamRobFloat2", 20.5)
					data <- cbSetCurrValue(data, "gamRobFloat3", 0.99)
					data <- cbSetCurrValue(data, "gamRobFloat4", 0.4)
					data <- cbSetCurrValue(data, "gamRobFloat5", 0.4)
					data <- cbSetCurrValue(data, "gamRobFloat6", 0.0001)

					data <- cbSetEnableFlag(data, "gamRobFloat1", T)
					data <- cbSetEnableFlag(data, "gamRobFloat2", T)
					data <- cbSetEnableFlag(data, "gamRobFloat3", T)
					data <- cbSetEnableFlag(data, "gamRobFloat4", T)
					data <- cbSetEnableFlag(data, "gamRobFloat5", T)
					data <- cbSetEnableFlag(data, "gamRobFloat6", T)
					data <- cbSetEnableFlag(data, "gamRobInteger1", F)
					data <- cbSetEnableFlag(data, "gamRobInteger2", F)
					data <- cbSetEnableFlag(data, "gamRobInteger3", F)
					data <- cbSetEnableFlag(data, "gamRobInteger4", F)
					data <- cbSetEnableFlag(data, "gamRobString", F)
				}

				else {
					data <- cbSetPrompt(data, "gamRobFloat1", "b1:")
					data <- cbSetPrompt(data, "gamRobFloat2", "b2:")
					data <- cbSetPrompt(data, "gamRobFloat3", "tol:")
					data <- cbSetPrompt(data, "gamRobFloat4", "til:")
					data <- cbSetPrompt(data, "gamRobFloat5", "sigma:")

					data <- cbSetCurrValue(data, "gamRobFloat1", 1.5)
					data <- cbSetCurrValue(data, "gamRobFloat2", 1.7)
					data <- cbSetCurrValue(data, "gamRobFloat3", 0.0001)
					data <- cbSetCurrValue(data, "gamRobFloat4", 0.001)
					data <- cbSetCurrValue(data, "gamRobFloat5", 0)

					data <- cbSetEnableFlag(data, "gamRobFloat1", T)
					data <- cbSetEnableFlag(data, "gamRobFloat2", T)
					data <- cbSetEnableFlag(data, "gamRobFloat3", T)
					data <- cbSetEnableFlag(data, "gamRobFloat4", T)
					data <- cbSetEnableFlag(data, "gamRobFloat5", T)
					data <- cbSetEnableFlag(data, "gamRobFloat6", F)
					data <- cbSetEnableFlag(data, "gamRobInteger1", T)
					data <- cbSetEnableFlag(data, "gamRobInteger2", F)
					data <- cbSetEnableFlag(data, "gamRobInteger3", F)
					data <- cbSetEnableFlag(data, "gamRobInteger4", T)
					data <- cbSetEnableFlag(data, "gamRobString", T)
				}
			}
			,
			LogNormal = {
				data <- cbSetPrompt(data, "gamRobFloat1", "alpha 1:")
				data <- cbSetPrompt(data, "gamRobFloat2", "alpha 2:")
				data <- cbSetPrompt(data, "gamRobFloat3", "u:")
				data <- cbSetPrompt(data, "gamRobFloat4", "beta:")
				data <- cbSetPrompt(data, "gamRobFloat5", "gam:")
				data <- cbSetPrompt(data, "gamRobFloat6", "tol:")

				data <- cbSetCurrValue(data, "gamRobFloat1", 0.5)
				data <- cbSetCurrValue(data, "gamRobFloat2", 20.5)
				data <- cbSetCurrValue(data, "gamRobFloat3", 0.99)
				data <- cbSetCurrValue(data, "gamRobFloat4", 0.4)
				data <- cbSetCurrValue(data, "gamRobFloat5", 0.4)
				data <- cbSetCurrValue(data, "gamRobFloat6", 0.0001)

				data <- cbSetEnableFlag(data, "gamRobFloat1", T)
				data <- cbSetEnableFlag(data, "gamRobFloat2", T)
				data <- cbSetEnableFlag(data, "gamRobFloat3", T)
				data <- cbSetEnableFlag(data, "gamRobFloat4", T)
				data <- cbSetEnableFlag(data, "gamRobFloat5", T)
				data <- cbSetEnableFlag(data, "gamRobFloat6", T)
				data <- cbSetEnableFlag(data, "gamRobInteger1", F)
				data <- cbSetEnableFlag(data, "gamRobInteger2", F)
				data <- cbSetEnableFlag(data, "gamRobInteger3", F)
				data <- cbSetEnableFlag(data, "gamRobInteger4", F)
				data <- cbSetEnableFlag(data, "gamRobString", F)
			}
		)
	
		data
	}


  if(cbIsInitDialogMessage(data)) {
		data <- cbSetEnableFlag(data, "gamRobString", F)
		data <- cbSetEnableFlag(data, "gamRobInteger1", F)
		data <- cbSetEnableFlag(data, "gamRobInteger2", F)
		data <- cbSetEnableFlag(data, "gamRobInteger3", F)
		data <- cbSetEnableFlag(data, "gamRobInteger4", F)
		data <- cbSetEnableFlag(data, "gamRobVariables", F)
		data <- cbSetPrompt(data, "SPropOmitMissing", "Omit Missing Values")
  }

  if(cbIsUpdateMessage(data)) {

		activeprop <- cbGetActiveProp(data)
		activevalue <- cbGetCurrValue(data, activeprop)

		if(activeprop == "SPropDataFrameList") {
			if(exists(cbGetCurrValue(data, "SPropDataFrameList")))
				col.names <- cbGetColumnNamesString(activevalue)
			else
				col.names <- ""

			data <- cbSetOptionList(data, "gamRobVariables", col.names)
		}

		if(exists(data.name <- cbGetCurrValue(data, "SPropDataFrameList"))) {
			data.class <- call("class", x = get(data.name))
			data.class <- eval(data.class)
		}
		else
			data.class <- NULL

		if(length(data.class) && data.class == "data.frame")
			data <- cbSetEnableFlag(data, "gamRobVariables", T)
		else
			data <- cbSetEnableFlag(data, "gamRobVariables", F)

		if(cbGetCurrValue(data, "gamRobDistn") == "LogNormal") {
			data <- cbSetCurrValue(data, "gamRobEstim", "tdmean")
			data <- cbSetOptionList(data, "gamRobEstim", c("tdmean"))
		}
		else
			data <- cbSetOptionList(data, "gamRobEstim", "tdmean, M")

		if(is.element(activeprop, c("gamRobDistn", "gamRobEstim")) && 
			cbGetCurrValue(data, "gamRobFittingMethod") != "MLE")
				data <- setAdvanced(data)

		if(activeprop == "gamRobFittingMethod") {
			if(activevalue == "Robust") {
				data <- cbSetEnableFlag(data, "gamRobMLEMaxIt", F)
				data <- cbSetEnableFlag(data, "gamRobMLETol", F)
				data <- cbSetEnableFlag(data, "gamRobEstim", T)
				data <- cbSetEnableFlag(data, "gamRobLogical", T)
				data <- setAdvanced(data)
			}
			else if(activevalue == "MLE") {
				data <- cbSetEnableFlag(data, "gamRobMLEMaxIt", T)
				data <- cbSetEnableFlag(data, "gamRobMLETol", T)
				data <- cbSetEnableFlag(data, "gamRobEstim", F)
				data <- cbSetEnableFlag(data, "gamRobFloat1", F)
				data <- cbSetEnableFlag(data, "gamRobFloat2", F)
				data <- cbSetEnableFlag(data, "gamRobFloat3", F)
				data <- cbSetEnableFlag(data, "gamRobFloat4", F)
				data <- cbSetEnableFlag(data, "gamRobFloat5", F)
				data <- cbSetEnableFlag(data, "gamRobFloat6", F)
				data <- cbSetEnableFlag(data, "gamRobInteger1", F)
				data <- cbSetEnableFlag(data, "gamRobInteger2", F)
				data <- cbSetEnableFlag(data, "gamRobInteger3", F)
				data <- cbSetEnableFlag(data, "gamRobInteger4", F)
				data <- cbSetEnableFlag(data, "gamRobLogical", F)
				data <- cbSetEnableFlag(data, "gamRobString", F)
			}
			else {
				data <- cbSetEnableFlag(data, "gamRobMLEMaxIt", T)
				data <- cbSetEnableFlag(data, "gamRobMLETol", T)
				data <- cbSetEnableFlag(data, "gamRobEstim", T)
				data <- cbSetEnableFlag(data, "gamRobFloat1", T)
				data <- cbSetEnableFlag(data, "gamRobFloat2", T)
				data <- cbSetEnableFlag(data, "gamRobFloat3", T)
				data <- cbSetEnableFlag(data, "gamRobFloat4", T)
				data <- cbSetEnableFlag(data, "gamRobFloat5", T)
				data <- cbSetEnableFlag(data, "gamRobFloat6", T)
				data <- cbSetEnableFlag(data, "gamRobInteger1", T)
				data <- cbSetEnableFlag(data, "gamRobInteger2", T)
				data <- cbSetEnableFlag(data, "gamRobInteger3", T)
				data <- cbSetEnableFlag(data, "gamRobInteger4", T)
				data <- cbSetEnableFlag(data, "gamRobLogical", T)
				data <- cbSetEnableFlag(data, "gamRobString", T)
				data <- setAdvanced(data)
			}
		}
	}

  if(cbIsRollbackMessage(data)) {
		if(cbGetCurrValue(data, "gamRobFittingMethod") == "MLE") {
				data <- cbSetEnableFlag(data, "gamRobMLEMaxIt", T)
				data <- cbSetEnableFlag(data, "gamRobMLETol", T)
				data <- cbSetEnableFlag(data, "gamRobEstim", F)
				data <- cbSetEnableFlag(data, "gamRobFloat1", F)
				data <- cbSetEnableFlag(data, "gamRobFloat2", F)
				data <- cbSetEnableFlag(data, "gamRobFloat3", F)
				data <- cbSetEnableFlag(data, "gamRobFloat4", F)
				data <- cbSetEnableFlag(data, "gamRobFloat5", F)
				data <- cbSetEnableFlag(data, "gamRobFloat6", F)
				data <- cbSetEnableFlag(data, "gamRobInteger1", F)
				data <- cbSetEnableFlag(data, "gamRobInteger2", F)
				data <- cbSetEnableFlag(data, "gamRobInteger3", F)
				data <- cbSetEnableFlag(data, "gamRobInteger4", F)
				data <- cbSetEnableFlag(data, "gamRobLogical", F)
				data <- cbSetEnableFlag(data, "gamRobString", F)
		}
		else
			data <- setAdvanced(data)

		if(exists(cbGetCurrValue(data, "SPropDataFrameList")))
			col.names <- cbGetColumnNamesString(
											cbGetCurrValue(data, "SPropDataFrameList"))
		else
			col.names <- ""

		data <- cbSetOptionList(data, "gamRobVariables", col.names)
		

		if(exists(data.name <- cbGetCurrValue(data, "SPropDataFrameList"))) {
			data.class <- call("class", x = get(data.name))
			data.class <- eval(data.class)
		}
		else
			data.class <- NULL

		if(length(data.class) && data.class == "data.frame")
			data <- cbSetEnableFlag(data, "gamRobVariables", T)
		else
			data <- cbSetEnableFlag(data, "gamRobVariables", F)
	}

  data
}

#
# end backGamRob
#
