"backDiscRob" <- function(data)
{
	setCV <- function(data)
	{
		sCovStruct <- cbGetCurrValue(data, "DiscRobCovStruct")
		sFamily <- cbGetCurrValue(data, "DiscRobFamily")
		sMethod <- cbGetCurrValue(data, "DiscRobFittingMethodBox")
		bEnable <- if(sFamily == "classical") (sCovStruct == "homoscedastic" ||
				sCovStruct == "heteroscedastic" || sCovStruct == 
				"spherical" || sCovStruct == "group spherical") else if(
			sFamily == "canonical")
			T
		else F
		if(sMethod != "MLE")
			bEnable <- F
		bEnable <- bEnable && is.all.white(cbGetCurrValue(data, "SPropWeights")) &&
			is.all.white(cbGetCurrValue(data, "SPropFrequencies"))
		if(!bEnable)
			data <- cbSetCurrValue(data, "DiscRobCrossValidate", F)
		data <- cbSetEnableFlag(data, "DiscRobCrossValidate", bEnable)
		return(data)
	}

	setCovList <- function(data, family)
	{
		sOptList <- character(0)
		sDefStruct <- cbGetCurrValue(data, "DiscRobCovStruct")
		sMethod <- cbGetCurrValue(data, "DiscRobFittingMethodBox")
		bEnableCV <- F
		bEnablePredict <- T
		switch(family,
			classical = {
				sOptList <- if(sMethod == "MLE") 
					"homoscedastic,spherical,proportional,group spherical,equal correlation,heteroscedastic"
					else "homoscedastic,heteroscedastic"
				if(!(k <- match(sDefStruct, unlist(unpaste(sOptList, sep
					 = ",")), nomatch = 0)))
					sDefStruct <- "homoscedastic"
				if( sMethod == "MLE") bEnableCV <- T
			}
			,
			"principal component" = {
				sOptList <- "proportional,common principal component"
				if(!(k <- match(sDefStruct, unlist(unpaste(sOptList, sep
					 = ",")), nomatch = 0)))
					sDefStruct <- "common principal component"
			}
			,
			canonical = {
				sOptList <- "homoscedastic"
				sDefStruct <- "homoscedastic"
				bEnableCV <- T
				bEnablePredict <- F
			}
			)
		# the next line gives an error when family = ppal component
		# and fitting method = "MLE" ... 
		data <- cbSetOptionList(data, "DiscRobCovStruct", sOptList)
		if(!cbIsRollbackMessage(data)) {
			data <- cbSetCurrValue(data, "DiscRobCovStruct", sDefStruct)
			if(!bEnableCV)
				data <- cbSetCurrValue(data, "DiscRobCrossValidate", "F")
			if(!bEnablePredict) {
				data <- cbSetCurrValue(data, "DiscRobPredictive", "F")
				data <- cbSetCurrValue(data, "DiscRobUnbiased", "F")
			}
		}
		data <- cbSetEnableFlag(data, "DiscRobCrossValidate", bEnableCV)
		data <- cbSetEnableFlag(data, "DiscRobPredictive", bEnablePredict)
		data <- cbSetEnableFlag(data, "DiscRobUnbiased", bEnablePredict)
		return(data)
	}



	initialmsg <- cbIsInitDialogMessage(data)
	rollbackmsg <- cbIsRollbackMessage(data)
	activeprop <- cbGetActiveProp(data)
	activevalue <- cbGetCurrValue(data, activeprop)

	if(initialmsg || rollbackmsg) {
		if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
			data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
			list.x <- paste("<ALL>", cbGetOptionList(data, "SPropDependent"),
				sep = ",")
			data <- cbSetOptionList(data, "SPropIndependent", list.x)
			data <- setCV(data)
		}
		# Initialize Dependent and Independent current values
		# Enhance to fill based on selected cols
		if(!rollbackmsg) {
			data <- cbSetCurrValue(data, "SPropDependent", "")
			data <- cbSetCurrValue(data, "SPropIndependent", "")
			data <- cbSetCurrValue(data, "SPropPrintLong", "T")
		}

		sMethod <- cbGetCurrValue(data, "DiscRobFittingMethodBox")
		if(sMethod == "MLE + Robust") {
			data <- cbSetEnableFlag(data, "DiscRobPlot", F)
			data <- cbSetCurrValue(data, "DiscRobPlot", F)
		}
		else
			data <- cbSetEnableFlag(data, "DiscRobPlot", T)				

		data <- setCovList(data, cbGetCurrValue(data, "DiscRobFamily"))
	}

	if(initialmsg)
	{
		data <- cbSetCurrValue(data, "SPropCovRobMCD", "Auto")
		data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
		data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
		data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
		data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
		data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
		data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
		data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
		data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
		data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
		data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
	}

	warn <- options("warn")
	on.exit(options(warn))

	switch(activeprop,
		SPropDataFrameList = {
			# Initialize Dependent and Independent variable lists
			if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
				data <- spropColName(data, "SPropDataFrameList", 
					"SPropDependent")
				list.x <- paste("<ALL>", cbGetOptionList(data, 
					"SPropDependent"), sep = ",")
				data <- cbSetOptionList(data, "SPropIndependent", list.x)
			}
			# Initialize Dependent and Independent current values
			# Enhance to fill based on selected cols
			data <- cbSetCurrValue(data, "SPropDependent", "")
			data <- cbSetCurrValue(data, "SPropIndependent", "")
		}
		,
		SPropDependent = {
			# Change response in formula to selected variable
			new.formula <- spropMakeFormula(cbGetCurrValue(data, 
				"SPropPFFormula"), cbGetCurrValue(data, "SPropDependent"),
				)
			data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
		}
		,
		SPropIndependent = {
			# Change predictors in formula to selected variables
			new.formula <- spropMakeFormula(cbGetCurrValue(data, 
				"SPropPFFormula"),  , cbGetCurrValue(data, 
				"SPropIndependent"))
			data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
		}
		,
		SPropPFFormula = {
			# Clear dependent and independent current values
			data <- cbSetCurrValue(data, "SPropDependent", "")
			data <- cbSetCurrValue(data, "SPropIndependent", "")
		}
		,
		DiscRobFamily = {
			data <- setCovList(data, cbGetCurrValue(data, "DiscRobFamily"))
			data <- setCV(data)
		}
		,
		DiscRobCovStruct = {
			data <- setCV(data)
		}
		,
		SPropWeights = {
			data <- setCV(data)
		}
		,
		SPropFrequencies = {
			data <- setCV(data)
		}
		,
		SPropPFButton = {
			if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
				### The current dialog ID is saved to a temporary global property, SPropCFFormulaID.
				### This SPropCFFormulaID is used in the formula builder dialog as an argument with control=Invisible.
				### The current data frame is saved to a temporary global property, SPropCFData.
				### This SPropCFData is used in the formula builder as an argument but with control= Invisible.
				### A data frame is required to launch the formula builder.
				### Only one formula builder is up at a time.(Not implemented yet)
				guiModify(classname = "Property", Name = 
					"SPropCFFormulaID", DialogControl = "Invisible",
					DefaultValue = cbGetDialogId(data))
				guiModify(classname = "Property", Name = "SPropCFData",
					DialogControl = "Invisible", DefaultValue = 
					cbGetCurrValue(data, "SPropDataFrameList"))
				guiModify(classname = "Property", Name = "SPropCFFormula",
					DefaultValue = cbGetCurrValue(data, 
					"SPropPFFormula"))
				guiDisplayDialog(classname = "Function", Name = 
					"makeFormulaTree")
				data <- cbSetEnableFlag(data, "SPropDataFrameList", F)
				data <- cbSetEnableFlag(data, "SPropPFFormula", F)
				data <- cbSetEnableFlag(data, "SPropPFButton", F)
				# 			Clear dependent and independent current values
				data <- cbSetCurrValue(data, "SPropDependent", "")
				data <- cbSetCurrValue(data, "SPropIndependent", "")
				# 			Disable dependent and independent fields
				data <- cbSetEnableFlag(data, "SPropDependent", F)
				data <- cbSetEnableFlag(data, "SPropIndependent", F)
			}
			else guiDisplayMessageBox(
					"Data Set doesn't exist.  Enter Data Set before building formula."
					)
		}
		,
		SPropPFEnableButton = {
			data <- cbSetEnableFlag(data, "SPropDataFrameList", T)
			data <- cbSetEnableFlag(data, "SPropPFFormula", T)
			data <- cbSetEnableFlag(data, "SPropPFButton", T)
			# 		Reenable Dependent and Independent
			data <- cbSetEnableFlag(data, "SPropDependent", T)
			data <- cbSetEnableFlag(data, "SPropIndependent", T)
		}
		,
		DiscRobFittingMethodBox = {

			sMethod <- cbGetCurrValue(data, "DiscRobFittingMethodBox")

			if(sMethod == "MLE + Robust") {
				data <- cbSetEnableFlag(data, "DiscRobPlot", F)
				data <- cbSetCurrValue(data, "DiscRobPlot", F)
			}
			else
				data <- cbSetEnableFlag(data, "DiscRobPlot", T)				

			sOptList <- if(sMethod != "MLE") "classical" else
					"classical,principal component,canonical"
			data <- cbSetOptionList(data, "DiscRobFamily", sOptList)
			data <- setCovList(data, cbGetCurrValue(data, "DiscRobFamily"))

			if (activevalue == "MLE") {
				data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
				data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
				data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
				data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
				data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
				data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
				data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
				data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
				data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
				data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
				data <- cbSetEnableFlag(data, "SPropCovRobMCD", F)

			}
			else {
				data <- cbSetEnableFlag(data, "SPropCovRobMCD", T)
				switch(cbGetCurrValue(data, "SPropCovRobMCD"),
					Auto = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
					}
					,
					pairwiseGK = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
					}
					,
					pairwiseQC = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
					}
					,
					M = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", T)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", T)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", T)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", T)
						}
						,
					MCD = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", T)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", T)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
						}
						,
					"Donoho-Stahel" = {
						data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
						data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", T)
						data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", T)
						data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrOne", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrThree", T)
						data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
						data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
						}
					)
				}
		}
		,
	"SPropCovRobMCD" = {
		switch(activevalue,
				Auto = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
				}
				,
				pairwiseGK = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
				}
				,
				pairwiseQC = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
				}
				,
				M = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", T)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", T)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", T)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", T)

					data <- cbSetPrompt(data, "SPropCovRobStrOne", "Contamination:")
					data <- cbSetPrompt(data, "SPropCovRobStrTwo", "Alpha:")
					data <- cbSetPrompt(data, "SPropCovRobStrThree", "tau:")
					data <- cbSetPrompt(data, "SPropCovRobStrFour", "tolerance:")
					data <- cbSetPrompt(data, "SPropCovRobInt", "Max Iterations:")

					data <- cbSetCurrValue(data, "SPropCovRobStrOne", .45)
					data <- cbSetCurrValue(data, "SPropCovRobStrTwo", .05)
					data <- cbSetCurrValue(data, "SPropCovRobStrThree", 1e-006)
					data <- cbSetCurrValue(data, "SPropCovRobStrFour", .001)
					data <- cbSetCurrValue(data, "SPropCovRobInt", 150)
				}
				,
				MCD = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", T)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", T)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", F)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", F)
				}
				,
				"Donoho-Stahel" = {
					data <- cbSetEnableFlag(data, "SPropCovRobQun", F)
					data <- cbSetEnableFlag(data, "SPropCovRobTrl", F)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntOne", T)
					data <- cbSetEnableFlag(data, "SPropCovRobAutoIntTwo", T)
					data <- cbSetEnableFlag(data, "SPropCovRobDSRandom", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrOne", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrTwo", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrThree", T)
					data <- cbSetEnableFlag(data, "SPropCovRobStrFour", F)
					data <- cbSetEnableFlag(data, "SPropCovRobInt", F)

					data <- cbSetPrompt(data, "SPropCovRobStrOne", "Tune:")
					data <- cbSetPrompt(data, "SPropCovRobStrTwo", "Prob:")
					data <- cbSetPrompt(data, "SPropCovRobStrThree", "Contamination:")
					data <- cbSetPrompt(data, "SPropCovRobStrFour", "")
					data <- cbSetPrompt(data, "SPropCovRobInt", "")

					data <- cbSetCurrValue(data, "SPropCovRobStrOne", .95)
					data <- cbSetCurrValue(data, "SPropCovRobStrTwo", .99)
					data <- cbSetCurrValue(data, "SPropCovRobStrThree", .5)
					data <- cbSetCurrValue(data, "SPropCovRobStrFour", "")
					data <- cbSetCurrValue(data, "SPropCovRobInt", "")
				}
			)
		}
	)
	data
}



