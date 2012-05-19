backAovRob <- function(data)
{
	activeprop <- cbGetActiveProp(data)
	activevalue <- cbGetCurrValue(data, activeprop)
	
	if(cbIsInitDialogMessage(data) ||
		((activeprop == "SPropDataFrameList") && cbIsUpdateMessage(data)) ||
		cbIsRollbackMessage(data))
		{
		data <- spropColName(data, "SPropDataFrameList", 
			"SPropContrastsVar")
		data <- spropEnable(data, c("SPropPlotPartialFit", 
			"SPropPlotPartialRugplot", "SPropPlotPartialScale"))
	# Initialize Dependent and Independent variable lists
		if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
			data <- spropColName(data, "SPropDataFrameList", 
				"SPropDependent")
			list.x <- paste("<ALL>", cbGetOptionList(data, 
				"SPropDependent"), sep = ",")
			data <- cbSetOptionList(data, "SPropIndependent", 
				list.x)
		}
	# Initialize Dependent and Independent current values
		if(!cbIsRollbackMessage(data)) {
			data <- cbSetCurrValue(data, "SPropDependent", "")
			data <- cbSetCurrValue(data, "SPropIndependent", "")
		}
}

if(cbIsInitDialogMessage(data)) {

		##	
		##	These results not yet supported
		##	
		
		data <- cbSetEnableFlag(data, "SPropPartialSSP", F)
				
		data <- cbSetEnableFlag(data, "SPropPlotPartialResid", T)
		data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
		data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
		data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)

		data <- cbSetCurrValue(data, "SPropPlotQQ", T)
		data <- cbSetCurrValue(data, "SPropPlotRDen", T)
		data <- cbSetCurrValue(data, "SPropPlotTime", T)

		##	
		## Set options for final estimate
		##
		
		data <- cbSetCurrValue(data, "SPropOptFinal", "MM")
		data <- cbSetCurrValue(data, "SPropOptLoss", "Optimal")
		data <- cbSetCurrValue(data, "SPropOptEff", "0.9")
		data <- cbSetCurrValue(data, "SPropOptMaxCoef", "50")
		data <- cbSetEnableFlag(data, "SPropOptMaxCoef", T)
		data <- cbSetCurrValue(data, "SPropOptMaxScale", "50")
		data <- cbSetCurrValue(data, "SPropOptMaxRefine", "50")
		data <- cbSetCurrValue(data, "SPropOptConvergeTol", "0.0001")
		data <- cbSetCurrValue(data, "SPropOptScaleTol", "1e-6")
		data <- cbSetCurrValue(data, "SPropOptRankTol", "1.5e-6")
		
		##
		## Disable all initial estimator options
		##
		
		data <- cbSetCurrValue(data, "SPropOptInitial", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptInitial", F)
		data <- cbSetCurrValue(data, "SPropOptSub", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptSub", F)
		data <- cbSetCurrValue(data, "SPropOptSeed", "1313")
		data <- cbSetEnableFlag(data, "SPropOptSeed", F)
		data <- cbSetCurrValue(data, "SPropOptPop", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptPop", F)
		data <- cbSetCurrValue(data, "SPropOptMut", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptMut", F)
		data <- cbSetCurrValue(data, "SPropOptBth", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptBth", F)
		data <- cbSetCurrValue(data, "SPropOptMax", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptMax", F)
		data <- cbSetCurrValue(data, "SPropOptStk", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptStk", F)
		data <- cbSetCurrValue(data, "SPropOptPrb", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptPrb", F)
	}
	
	
	if(cbIsUpdateMessage(data)) {
		if(activeprop == "SPropContrastsList")
			data <- cbSetCurrValue(data, "SPropContrastsVar", "")
		else if(activeprop == "SPropContrastsVar" && activevalue != " ")
			{
			if((curr.contrast <- cbGetCurrValue(data, "SPropContrastsList")) == " ")
				guiCreate("MessageBox", String = 
					"Select a contrast first then variable(s).")
			else {
				activevalue <- unlist(unpaste(as.name(activevalue), sep = ","))
				newvar <- activevalue
				switch(curr.contrast,
					Helmert = {
						activevalue <- paste(paste(activevalue, "=contr.helmert",
							sep = ""), collapse = ",")
					}
					,
					"Orthogonal Polynomial" = {
						activevalue <- paste(paste(activevalue, "=contr.poly",
							sep = ""), collapse = ",")
					}
					,
					Sum = {
						activevalue <- paste(paste(activevalue, "=contr.sum",
							sep = ""), collapse = ",")
					}
					,
					Treatment = {
						activevalue <- paste(paste(activevalue, "=contr.treatment",
							sep = ""), collapse = ",")
					}
					)
				curr.list <- cbGetCurrValue(data, "SPropContrasts")
				if(nchar(curr.list) != 0) {
					curr.list <- unpaste(unlist(unpaste(
						curr.list, sep = ",")), sep = "=")
					oldvar <- curr.list[[1]]
					ind <- is.na(match(oldvar, newvar))
					if(any(ind)) {
						new.list <- paste(paste(curr.list[[1]][ind],
							curr.list[[2]][ind], sep = "="), collapse = ",")
						activevalue <- paste(new.list, as.name(activevalue), sep = ",")
					}
				}
				data <- cbSetCurrValue(data, "SPropContrasts", activevalue)
			}
		}
		switch(activeprop,
			SPropDataFrameList = {
				data <- spropColName(data, "SPropDataFrameList", "SPropContrastsVar")
				if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
					data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
					list.x <- paste("<ALL>", cbGetOptionList(data, "SPropDependent"), sep = ",")
					data <- cbSetOptionList(data, "SPropIndependent", list.x)
				}
				data <- cbSetCurrValue(data, "SPropPFFormula", "")
				if(!cbIsRollbackMessage(data)) {
					data <- cbSetCurrValue(data, "SPropDependent", "")
					data <- cbSetCurrValue(data, "SPropIndependent", "")
				}
			}
			,
			SPropPlotQQ = {
				if(activevalue == "T") {
					data <- cbSetEnableFlag(data, "SPropPlotQQEnvelope",  T)
					data <- cbSetEnableFlag(data, "SPropPlotRobQQLine",  T)
					data <- cbSetEnableFlag(data, "SPropPlotHalfNormal",  T)
				}
				else {
					data <- cbSetEnableFlag(data, "SPropPlotQQEnvelope",  F)
					data <- cbSetEnableFlag(data, "SPropPlotRobQQLine",  F)
					data <- cbSetEnableFlag(data, "SPropPlotHalfNormal",  F)
				}
			}
			,
			SPropOptFinal = {
				if(activevalue == "MM") {
					data <- cbSetEnableFlag(data, "SPropOptMaxCoef", T)
					data <- cbSetEnableFlag(data, "SPropOptLoss", T)
					data <- cbSetEnableFlag(data, "SPropOptEff", T)
				}
				else {
					data <- cbSetEnableFlag(data, "SPropOptMaxCoef", F)
					data <- cbSetEnableFlag(data, "SPropOptLoss", F)
					data <- cbSetEnableFlag(data, "SPropOptEff", F)
				}
			}
			,
			SPropDependent = {
				# Change response in formula to selected variable
				new.formula <- spropMakeFormula(cbGetCurrValue(data, "SPropPFFormula"),
									cbGetCurrValue(data, "SPropDependent"),  )
				data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
			}
			,
			SPropIndependent = {
				# Change predictors in formula to selected variables
				new.formula <- spropMakeFormula(cbGetCurrValue(
					data, "SPropPFFormula"),  , cbGetCurrValue(data, "SPropIndependent"))
				data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
			}
			,
			SPropPFButton = {
				if(exists(cbGetCurrValue(data, "SPropDataFrameList")))
					{
					### The current dialog ID is saved to a temporary global property, SPropCFFormulaID.
					### This SPropCFFormulaID is used in the formula builder dialog as an argument with control=Invisible.
					### The current data frame is saved to a temporary global property, SPropCFData.
					### This SPropCFData is used in the formula builder as an argument but with control= Invisible.
					### A data frame is required to launch the formula builder.
					### Only one formula builder is up at a time.
					guiModify(classname = "Property",
						Name = "SPropCFFormulaID", 
						DialogControl = "Invisible",
						DefaultValue = cbGetDialogId(
						data))
					guiModify(classname = "Property",
						Name = "SPropCFData",
						DialogControl = "Invisible",
						DefaultValue = cbGetCurrValue(data, "SPropDataFrameList"))
					guiModify(classname = "Property",
						Name = "SPropCFFormula", 
						DefaultValue = cbGetCurrValue(data, "SPropPFFormula"))
					guiDisplayDialog(classname = "Function",
						Name = "makeFormulaAov")
					data <- cbSetEnableFlag(data, "SPropDataFrameList", F)
					data <- cbSetEnableFlag(data, "SPropPFFormula", F)
					data <- cbSetEnableFlag(data, "SPropPFButton", F)
					# Clear dependent and independent current values
					data <- cbSetCurrValue(data, "SPropDependent", "")
					data <- cbSetCurrValue(data, "SPropIndependent", "")
					# Disable dependent and independent fields
					data <- cbSetEnableFlag(data, "SPropDependent", F)
					data <- cbSetEnableFlag(data, "SPropIndependent", F)
				}
				else guiCreate("MessageBox", String = 
						"Data Frame doesn't exit.  Enter Data Frame before building formula.")
			}
			,
			SPropPFEnableButton = {
				data <- cbSetEnableFlag(data, "SPropDataFrameList", T)
				data <- cbSetEnableFlag(data, "SPropPFFormula", T)
				data <- cbSetEnableFlag(data, "SPropPFButton", T)
			# Reenable Dependent and Independent
				data <- cbSetEnableFlag(data, "SPropDependent", T)
				data <- cbSetEnableFlag(data, "SPropIndependent", T)
			}
			,
			SPropPFFormula = {
				sFormula <- cbGetCurrValue(data, "SPropPFFormula")
				bEnable <- T
				if(!is.all.white(sFormula, empty = T) &&
					(parse.test(paste(sFormula, ";", sep = "")) == "complete"))
					{
					sStack <- all.names(parse(text = sFormula))
					if(length(sStack) > 2 && sStack[1] == "~")
						{
						specials <- attr(terms(sFormula, c("cbind", "Error")), "specials")
						if(!is.null(specials))
							bEnable <- F
						else if(is.all.white(cbGetCurrValue(data, "SPropDataFrameList"), empty = T))
							{
							if(sStack[2] == "cbind")
								bEnable <- F
							else if(exists(sStack[2]))
								{
								response <- get(sStack[2])
								if(!is.null(response) &&!is.null(d <-dim(response)))
									bEnable <- (d[2] ==1)
								}
							}
						}
					}
				if(bEnable) {
					data <- cbSetEnableFlag(data, "SPropPlotResidVsFit", T)
					data <- cbSetEnableFlag(data, "SPropPlotSqrtAbsResid", T)
					data <- cbSetEnableFlag(data, "SPropPlotResponseVsFit", T)
					data <- cbSetEnableFlag(data, "SPropPlotQQ", T)
					data <- cbSetEnableFlag(data, "SPropPlotRFSpread", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialResid", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
				}
				else {
					data <- cbSetCurrValue(data, "SPropPlotResidVsFit", "F")
					data <- cbSetEnableFlag(data, "SPropPlotResidVsFit", F)
					data <- cbSetCurrValue(data, "SPropPlotSqrtAbsResid", "F")
					data <- cbSetEnableFlag(data, "SPropPlotSqrtAbsResid", F)
					data <- cbSetCurrValue(data, "SPropPlotResponseVsFit", "F")
					data <- cbSetEnableFlag(data, "SPropPlotResponseVsFit", F)
					data <- cbSetCurrValue(data, "SPropPlotQQ", "F")
					data <- cbSetEnableFlag(data, "SPropPlotQQ", F)
					data <- cbSetCurrValue(data, "SPropPlotRFSpread", "F")
					data <- cbSetEnableFlag(data, "SPropPlotRFSpread", F)
					data <- cbSetCurrValue(data, "SPropPlotPartialResid", "F")
					data <- cbSetEnableFlag(data, "SPropPlotPartialResid", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
				}
				data <- cbSetCurrValue(data, "SPropDependent","")
				data <- cbSetCurrValue(data, "SPropIndependent","")
			}
			,
			SPropRobFitRad = {
				if (activevalue == "LS") {
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
					
					data <- spropEnable(data, c("SPropOptFinal", "SPropOptLoss", 
						"SPropOptEff", "SPropOptMaxCoef", "SPropOptMaxScale",
						"SPropOptMaxRefine", "SPropOptConvergeTol", "SPropOptScaleTol",
						"SPropOptRankTol"))

				}
				else {
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", T)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", T)
					
					data <- spropEnable(data, c("SPropOptFinal", "SPropOptLoss", 
						"SPropOptEff", "SPropOptMaxCoef", "SPropOptMaxScale",
						"SPropOptMaxRefine", "SPropOptConvergeTol", "SPropOptScaleTol",
						"SPropOptRankTol"), c("SPropOptFinal", "SPropOptLoss", 
						"SPropOptEff", "SPropOptMaxCoef", "SPropOptMaxScale",
						"SPropOptMaxRefine", "SPropOptConvergeTol", "SPropOptScaleTol",
						"SPropOptRankTol"))
						
					if(cbGetCurrValue(data, "SPropOptFinal") == "Adaptive")
						data <- spropEnable(data, c("SPropOptMaxCoef",
							"SPropOptLoss", "SPropOptEff"))
					
				}
				
				if (activevalue == "Robust") {
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
				}
			}
			,
			SPropPlotPartialResid = {
				if(activevalue == "T") {
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
				}
				else {
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
				}
			}
		)
	}
	#end of is update
	
	if(cbIsRollbackMessage(data)) {
		activevalue <- cbGetCurrValue(data, "SPropOptFinal")
		if(activevalue == "MM") {
			data <- cbSetEnableFlag(data, "SPropOptMaxCoef", T)
			data <- cbSetEnableFlag(data, "SPropOptLoss", T)
			data <- cbSetEnableFlag(data, "SPropOptEff", T)
		}
		else {
			data <- cbSetEnableFlag(data, "SPropOptMaxCoef", F)
			data <- cbSetEnableFlag(data, "SPropOptMaxCoef", F)
			data <- cbSetEnableFlag(data, "SPropOptLoss", F)
			data <- cbSetEnableFlag(data, "SPropOptEff", F)
		}
	}
	data
}
