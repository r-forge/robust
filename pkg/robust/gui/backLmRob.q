"backLmRob" <- function(data)
{
	activeprop <- cbGetActiveProp(data)
	activevalue <- cbGetCurrValue(data, activeprop)
	if(cbIsInitDialogMessage(data)) {
		data <- cbSetCurrValue(data, "SPropRobFitRad", "LS + Robust")
		if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
			data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
			list.x <- paste("<ALL>", cbGetOptionList(data, 
				"SPropDependent"), sep = ",")
			data <- cbSetOptionList(data, "SPropIndependent", list.x)
		}

		# Initialize Dependent and Independent current values

		data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", F)
		data <- cbSetCurrValue(data, "SPropRobBackwards", F)
		data <- cbSetEnableFlag(data, "SPropRobBackwards", F)
		data <- cbSetCurrValue(data, "SPropDependent", "")
		data <- cbSetCurrValue(data, "SPropIndependent", "")
		data <- cbSetCurrValue(data, "SPropPlotQQ", "T")
		data <- cbSetCurrValue(data, "SPropPlotTime", "T")
		data <- cbSetCurrValue(data, "SPropPlotRDen", "T")
		data <- cbSetCurrValue(data, "SPropPlotRRRD", "T")
		data <- cbSetEnableFlag(data, "SPropPlotOverQ", T)
		data <- cbSetEnableFlag(data, "SPropPlotOverD", T)
		data <- cbSetCurrValue(data, "SPropPlotPartialResid", "F")
		data <- cbSetCurrValue(data, "SPropPlotPartialFit", "F")
		data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
		data <- cbSetCurrValue(data, "SPropPlotPartialRugplot", "F")
		data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
		data <- cbSetCurrValue(data, "SPropPlotPartialScale", "F")
		data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
		data <- cbSetCurrValue(data, "SPropPlotOneVar", "F")
		data <- cbSetEnableFlag(data, "SPropPlotOneVar", F)
		
		# Initialize Advanced Page

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
		
		data <- cbSetCurrValue(data, "SPropOptInitial", "Auto")
		data <- cbSetCurrValue(data, "SPropOptSub", "Auto")
		data <- cbSetCurrValue(data, "SPropOptSeed", "1313")

		data <- cbSetCurrValue( data, "SPropOptPop", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptPop", F)
		data <- cbSetCurrValue( data, "SPropOptMut", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptMut", F)
		data <- cbSetCurrValue( data, "SPropOptBth", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptBth", F)
		data <- cbSetCurrValue( data, "SPropOptMax", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptMax", F)
		data <- cbSetCurrValue( data, "SPropOptStk", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptStk", F)
		data <- cbSetCurrValue( data, "SPropOptPrb", "Auto")
		data <- cbSetEnableFlag(data, "SPropOptPrb", F)

	# Enable plot with data if only one Term

		temp <- cbGetCurrValue(data, "SPropPFFormula")
		if(!is.all.white(temp)){
			temp <- terms(as.formula(temp))
			if(length(attr(temp, "term.labels")) == 1)
				data <- cbSetEnableFlag(data, "SPropPlotOneVar", T)
		}
	}
	
	
	if(cbIsUpdateMessage(data))
		switch(activeprop,
			SPropDataFrameList = {
				if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
				  data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
				  list.x <- paste("<ALL>", cbGetOptionList(data,
				    "SPropDependent"), sep = ",")
				  data <- cbSetOptionList(data, "SPropIndependent", list.x)
				}

		# Initialize Dependent and Independent current values

				data <- cbSetCurrValue(data, "SPropDependent", "")
				data <- cbSetCurrValue(data, "SPropIndependent", "")
			}
			,

			SPropDependent = {

		# Change response in formula to selected variable

				new.formula <- spropMakeFormula(cbGetCurrValue(
				  data, "SPropPFFormula"), cbGetCurrValue(data, 
				  "SPropDependent"),  )
				data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
			}
			,

			SPropIndependent = {

		# Change predictors in formula to selected variables

				new.formula <- spropMakeFormula(cbGetCurrValue(
				  data, "SPropPFFormula"),  , cbGetCurrValue(
				  data, "SPropIndependent"))

				data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
				data.set <- cbGetCurrValue(data, "SPropDataFrameList")
				if(length(attr(terms(new.formula, data = eval(parse(text = data.set))),
										"term.labels")) == 1) {
					data <- cbSetEnableFlag(data, "SPropPlotOneVar", T)
				}

				else {
					data <- cbSetCurrValue(data,"SPropPlotOneVar", F)
					data <- cbSetEnableFlag(data, "SPropPlotOneVar", F)
				}
			}
			,

			SPropPFFormula = {

		# Clear dependent and independent current values

				data <- cbSetCurrValue(data, "SPropDependent", "")
				data <- cbSetCurrValue(data, "SPropIndependent", "")

		# Check for one variable

				data <- cbSetCurrValue(data,"SPropPlotOneVar", F)
				data <- cbSetEnableFlag(data, "SPropPlotOneVar", F)
			}
			,

			SPropPFButton = {

				if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
	## The current dialog ID is saved to a temporary global property, 
	## SPropCFFormulaID.  This is used in the formula builder dialog as
	## as an argument with control=Invisible.  The current data frame is
	## saved to a temporary global property, SPropCFData which is used in
	## the formula builder as an argument but with control=Invisible.
	## A data frame is required to launch the formula builder.  Only one
	## formula builder is up at a time.

					guiModify("Property", Name="SPropCFFormulaID", DialogControl= 
						"Invisible", DefaultValue=cbGetDialogId(data))
					guiModify("Property", Name="SPropCFData", DialogControl="Invisible", 
						DefaultValue=cbGetCurrValue(data, "SPropDataFrameList"))
					guiModify("Property", Name="SPropCFFormula", DefaultValue= 
						cbGetCurrValue(data, "SPropPFFormula"))
					guiDisplayDialog("Function", Name="makeFormulaLm", bModal=T)
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
				else 
					guiCreate("MessageBox", String = 
				    "Data Set doesn't exit. Enter Data Set before building formula.")
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

			SPropPrintLong = {
				if(activevalue == "F") {
					data <- cbSetEnableFlag(data, "SPropPrintCor", F)
					data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", F)
				}
				else {
					if(cbGetCurrValue(data, "SPropRobFitRad") == "Robust")
						data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", T)
					data <- cbSetEnableFlag(data, "SPropPrintCor", T)
					data <- cbSetCurrValue(data, "SPropPrintShort", "F")
				}
			}
			,

			SPropPrintShort = {
				if(activevalue == "T") {	
					data <- cbSetEnableFlag(data, "SPropPrintCor",  F)
					data <- cbSetCurrValue( data, "SPropPrintLong", "F")
				}
				else {
					data <- cbSetEnableFlag(data, "SPropPrintCor",  T)
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

			SPropPlotPartialResid = {
				if (activevalue == "T") {
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
					data <- cbSetCurrValue(data,  "SPropPlotPartialScale", "T")
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
				}
				else {
					data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
					data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
				}
			}
			,

			SPropRobFitRad = {
				if (activevalue == "LS") {
					data <- cbSetEnableFlag(data, "SPropRobButton", F)
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
					data <- cbSetEnableFlag(data, "SPropPlotCooks", T)
					data <- cbSetEnableFlag(data, "SPropRobBackwards", T)
					data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", F)
				
					data <- spropEnable(data, c("SPropOptEff", "SPropOptMaxCoef",
						"SPropOptMaxScale", "SPropOptMaxRefine", "SPropOptConvergeTol",
						"SPropOptScaleTol", "SPropOptRankTol", "SPropOptSub",
						"SPropOptSeed", "SPropOptInitial", "SPropOptFinal",
						"SPropOptLoss", "SPropOptPop", "SPropOptBth",
						"SPropOptMax", "SPropOptMut", "SPropOptStk",
						"SPropOptPrb"))

				}
				else {
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", T)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", T)
					data <- cbSetCurrValue(data, "SPropRobBackwards", F)
					data <- cbSetEnableFlag(data, "SPropRobBackwards", F)
					data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", F)
				
					data <- spropEnable(data, c("SPropOptEff", "SPropOptMaxCoef",
						"SPropOptMaxScale", "SPropOptMaxRefine", "SPropOptConvergeTol",
						"SPropOptScaleTol", "SPropOptRankTol", "SPropOptSub",
						"SPropOptSeed", "SPropOptInitial", "SPropOptFinal",
						"SPropOptLoss"), c("SPropOptEff", "SPropOptMaxCoef",
						"SPropOptMaxScale", "SPropOptMaxRefine", "SPropOptConvergeTol",
						"SPropOptScaleTol", "SPropOptRankTol", "SPropOptSub",
						"SPropOptSeed", "SPropOptInitial", "SPropOptFinal",
						"SPropOptLoss"))
						
					if(cbGetCurrValue(data, "SPropOptFinal") == "Adaptive")
						data <- spropEnable(data, c("SPropOptMaxCoef",
							"SPropOptLoss", "SPropOptEff"))
					
					method <- cbGetCurrValue(data, "SPropOptInitial")
					if(method == "Auto" || method == "Random") {
						data <- cbSetEnableFlag(data, "SPropOptSub", T)
						data <- cbSetEnableFlag(data, "SPropOptSeed", T)
						data <- cbSetEnableFlag(data, "SPropOptPop", F)
						data <- cbSetEnableFlag(data, "SPropOptMut", F)
						data <- cbSetEnableFlag(data, "SPropOptMax", F)
						data <- cbSetEnableFlag(data, "SPropOptBth", F)
						data <- cbSetEnableFlag(data, "SPropOptStk", F)
						data <- cbSetEnableFlag(data, "SPropOptPrb", F)
					}
					else if(method == "Genetic") {
						data <- cbSetEnableFlag(data, "SPropOptSub", T)
						data <- cbSetEnableFlag(data, "SPropOptSeed", T)
						data <- cbSetEnableFlag(data, "SPropOptPop", T)
						data <- cbSetEnableFlag(data, "SPropOptMut", T)
						data <- cbSetEnableFlag(data, "SPropOptMax", T)
						data <- cbSetEnableFlag(data, "SPropOptBth", T)
						data <- cbSetEnableFlag(data, "SPropOptStk", T)
						data <- cbSetEnableFlag(data, "SPropOptPrb", T)
					}
					else {
						data <- cbSetEnableFlag(data, "SPropOptSub", F)
						data <- cbSetEnableFlag(data, "SPropOptSeed", F)
						data <- cbSetEnableFlag(data, "SPropOptPop", F)
						data <- cbSetEnableFlag(data, "SPropOptMut", F)
						data <- cbSetEnableFlag(data, "SPropOptMax", F)
						data <- cbSetEnableFlag(data, "SPropOptBth", F)
						data <- cbSetEnableFlag(data, "SPropOptStk", F)
						data <- cbSetEnableFlag(data, "SPropOptPrb", F)
					}
				}
				
				if (activevalue == "Robust") {
					if(cbGetCurrValue(data, "SPropPrintLong") == "T")
						data <- cbSetEnableFlag(data, "gui_lmRob_summary_bootse", T)
					data <- cbSetEnableFlag(data, "SPropRobBackwards", T)
					data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
					data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
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

			SPropOptInitial = {
				if(activevalue == "Auto" || activevalue == "Random") {
					data <- cbSetEnableFlag(data, "SPropOptSub", T)
					data <- cbSetEnableFlag(data, "SPropOptSeed", T)
					data <- cbSetEnableFlag(data, "SPropOptPop", F)
					data <- cbSetEnableFlag(data, "SPropOptMut", F)
					data <- cbSetEnableFlag(data, "SPropOptMax", F)
					data <- cbSetEnableFlag(data, "SPropOptBth", F)
					data <- cbSetEnableFlag(data, "SPropOptStk", F)
					data <- cbSetEnableFlag(data, "SPropOptPrb", F)
				}
				else if(activevalue == "Genetic") {
					data <- cbSetEnableFlag(data, "SPropOptSub", T)
					data <- cbSetEnableFlag(data, "SPropOptSeed", T)
					data <- cbSetEnableFlag(data, "SPropOptPop", T)
					data <- cbSetEnableFlag(data, "SPropOptMut", T)
					data <- cbSetEnableFlag(data, "SPropOptMax", T)
					data <- cbSetEnableFlag(data, "SPropOptBth", T)
					data <- cbSetEnableFlag(data, "SPropOptStk", T)
					data <- cbSetEnableFlag(data, "SPropOptPrb", T)
				}
				else {
					data <- cbSetEnableFlag(data, "SPropOptSub", F)
					data <- cbSetEnableFlag(data, "SPropOptSeed", F)
					data <- cbSetEnableFlag(data, "SPropOptPop", F)
					data <- cbSetEnableFlag(data, "SPropOptMut", F)
					data <- cbSetEnableFlag(data, "SPropOptMax", F)
					data <- cbSetEnableFlag(data, "SPropOptBth", F)
					data <- cbSetEnableFlag(data, "SPropOptStk", F)
					data <- cbSetEnableFlag(data, "SPropOptPrb", F)
				}
			}
		)


	if(cbIsRollbackMessage(data)) {
		if(cbGetCurrValue(data, "SPropRobFitRad") == "LS + Robust")
			data <- cbSetEnableFlag(data, "SPropRobBackwards", F)
		else
			data <- cbSetEnableFlag(data, "SPropRobBackwards", T)

		if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
			data <- spropColName(data, "SPropDataFrameList", 
				"SPropDependent")
			list.x <- paste("<ALL>", cbGetOptionList(data, 
				"SPropDependent"), sep = ",")
			data <- cbSetOptionList(data, "SPropIndependent", 
				list.x)
		}
		activevalue <- cbGetCurrValue(data, "SPropPrintLong")
		if(activevalue == "F") {
			data <- cbSetEnableFlag(data, "SPropPrintCor", F)
		}
		else {	
			data <- cbSetEnableFlag(data, "SPropPrintCor", T)
			data <- cbSetCurrValue(data, "SPropPrintShort", "F")
		}
		activevalue <- cbGetCurrValue(data, "SPropPrintShort")
		if(activevalue == "F") {
			data <- cbSetEnableFlag(data, "SPropPrintCor", T)
		}
		else {
			data <- cbSetEnableFlag(data, "SPropPrintCor",  F)
			data <- cbSetCurrValue( data, "SPropPrintLong", "F")
		}
		activevalue <- cbGetCurrValue(data, "SPropRobFitRad")
		if (activevalue == "LS") {
			data <- cbSetEnableFlag(data, "SPropRobButton", F)
			data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
			data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
			data <- cbSetEnableFlag(data, "SPropPlotCooks", T)
		}
		else if (activevalue == "Robust") {
			data <- cbSetEnableFlag(data, "SPropRobButton", T)
			data <- cbSetEnableFlag(data, "SPropPlotOverQ", F)
			data <- cbSetEnableFlag(data, "SPropPlotOverD", F)
			data <- cbSetEnableFlag(data, "SPropPlotCooks", F)
		}
		else {
			data <- cbSetEnableFlag(data, "SPropRobButton", T)
			data <- cbSetEnableFlag(data, "SPropPlotOverQ", T)
			data <- cbSetEnableFlag(data, "SPropPlotOverD", T)
			data <- cbSetEnableFlag(data, "SPropPlotCooks", F)
		}
		activevalue <- cbGetCurrValue(data, "SPropPlotPartialResid")
		if (activevalue == "T") {
			data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
			data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
			data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
			data <- cbSetCurrValue(data, "SPropPlotPartialScale", "T")
		}
		else {
			data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
			data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
			data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
		}
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
		activevalue <- cbGetCurrValue(data, "SPropOptInitial")
		if(activevalue == "Auto" || activevalue == "Random") {
			data <- cbSetEnableFlag(data, "SPropOptSub", T)
			data <- cbSetEnableFlag(data, "SPropOptSeed", T)
			data <- cbSetEnableFlag(data, "SPropOptPop", F)
			data <- cbSetEnableFlag(data, "SPropOptMut", F)
			data <- cbSetEnableFlag(data, "SPropOptMax", F)
			data <- cbSetEnableFlag(data, "SPropOptBth", F)
			data <- cbSetEnableFlag(data, "SPropOptStk", F)
			data <- cbSetEnableFlag(data, "SPropOptPrb", F)
		}
		else if(activevalue == "Genetic") {
			data <- cbSetEnableFlag(data, "SPropOptSub", T)
			data <- cbSetEnableFlag(data, "SPropOptSeed", T)
			data <- cbSetEnableFlag(data, "SPropOptPop", T)
			data <- cbSetEnableFlag(data, "SPropOptMut", T)
			data <- cbSetEnableFlag(data, "SPropOptMax", T)
			data <- cbSetEnableFlag(data, "SPropOptBth", T)
			data <- cbSetEnableFlag(data, "SPropOptStk", T)
			data <- cbSetEnableFlag(data, "SPropOptPrb", T)
		}
		else {
			data <- cbSetEnableFlag(data, "SPropOptSub", F)
			data <- cbSetEnableFlag(data, "SPropOptSeed", F)
			data <- cbSetEnableFlag(data, "SPropOptPop", F)
			data <- cbSetEnableFlag(data, "SPropOptMut", F)
			data <- cbSetEnableFlag(data, "SPropOptMax", F)
			data <- cbSetEnableFlag(data, "SPropOptBth", F)
			data <- cbSetEnableFlag(data, "SPropOptStk", F)
			data <- cbSetEnableFlag(data, "SPropOptPrb", F)
		}
	}
	data
}
