#
# backGlmRob (based on backGlm)
#

backGlmRob <- function(data)
{
  if(cbIsInitDialogMessage(data)) {
    if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
      data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
      list.x <- paste("<ALL>", cbGetOptionList(data, "SPropDependent"), sep = ",")
      data <- cbSetOptionList(data, "SPropIndependent", list.x)
    }

# Initialize SPropMaxiter and SPropTolerance
    data <- cbSetCurrValue(data, "SPropMaxiter", "10")
    data <- cbSetCurrValue(data, "SPropTolerance", "0.0001")

# Initialize glmrobMaxiter and glmrobTolerance
    data <- cbSetCurrValue(data, "glmrobMaxiter", "50")
    data <- cbSetCurrValue(data, "glmrobTolerance", "0.001")

# Initialize family
    data <- cbSetCurrValue(data, "SPropGlmrobFamily", "binomial")
    data <- cbSetOptionList(data, "SPropGlmrobFamily", "binomial,poisson")
    data <- cbSetCurrValue(data, "SPropModelLink", "logit")
    data <- cbSetOptionList(data, "SPropModelLink", "logit")

# Initialize Method
    data <- cbSetCurrValue(data, "glmrobMethodBox", "MLE + Robust")

# Disable Predict SE option
#    data <- cbSetEnableFlag(data, "glmrobPredSaveSE", F)

# Enable Options Button
	data <- cbSetEnableFlag(data, "SPropGlmRobOptButton", T)
	
# Enable Comparison Plots
	data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", T)
	data <- cbSetEnableFlag(data, "gui_glmrob_overlaid_QQ", T)
	
# Disable Partial Plots Options
    data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
    data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
    data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)

# Initialize Dependent and Independent current values
    data <- cbSetCurrValue(data, "SPropDependent", "")
    data <- cbSetCurrValue(data, "SPropIndependent", "")

# Disable the Variance Function Option
    data <- cbSetEnableFlag(data, "SPropModelVariance", F)

# Initialize Advanced Page
	data <- cbSetOptionList(data, "glmrobFitMethod", "cubif,misclass,mallows")
	data <- cbSetOptionList(data, "glmrobMallowsFn", "huber,carroll")
	data <- cbSetCurrValue(data, "glmrobMallowsFn", "carroll")
	data <- cbSetCurrValue(data, "glmrobMallowsTuning", 8)
	data <- cbSetCurrValue(data, "glmrobMCgamma", "0.01")
	data <- cbSetCurrValue(data, "glmrobFitMethod", "cubif")
	
	#enable options
	data <- cbSetEnableFlag(data, "glmrobTolerance", T)
	data <- cbSetEnableFlag(data, "glmrobMaxiter", T)
	data <- cbSetEnableFlag(data, "glmrobCConst", T)
	data <- cbSetEnableFlag(data, "glmrobUfact", T)
	data <- cbSetEnableFlag(data, "glmrobTrace", T)
	
	#disable options
	data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
	data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
	data <- cbSetEnableFlag(data, "glmrobMCtol", F)
	data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
	data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
	data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
	data <- cbSetCurrValue(data, "glmrobMCtrace", "F")
	data <- cbSetCurrValue(data, "glmrobMaxiter", "50")
	data <- cbSetCurrValue(data, "glmrobTolerance", "0.001")
	data <- cbSetCurrValue(data, "glmrobCConst", "1.5")
	data <- cbSetCurrValue(data, "glmrobUfact", "2")
	data <- cbSetCurrValue(data, "glmrobMCmaxit", "30")
	data <- cbSetCurrValue(data, "glmrobMCtol", "0.001")
	data <- cbSetCurrValue(data, "glmrobMCtrace", "F")
  } # end of if( cbIsInitDialogMessage( ...



  if(cbIsUpdateMessage(data)) {
    activeprop <- cbGetActiveProp(data)
    activevalue <- cbGetCurrValue(data, activeprop)
    switch(activeprop,
      SPropDataFrameList = {
# Initialize Dependent and Independent variable lists
        if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
          data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
          list.x <- paste("<ALL>", cbGetOptionList(data, "SPropDependent"), sep = ",")
          data <- cbSetOptionList(data, "SPropIndependent", list.x)
        }

# Initialize Dependent and Independent current values
        data <- cbSetCurrValue(data, "SPropDependent", "")
        data <- cbSetCurrValue(data, "SPropIndependent", "")
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
        new.formula <- spropMakeFormula(cbGetCurrValue(data, "SPropPFFormula"),  , 
                                        cbGetCurrValue(data, "SPropIndependent"))
        data <- cbSetCurrValue(data, "SPropPFFormula", new.formula)
      }
      ,
      SPropPFFormula = {
# Clear dependent and independent current values
        data <- cbSetCurrValue(data, "SPropDependent", "")
        data <- cbSetCurrValue(data, "SPropIndependent", "")
		}
		,
		SPropGlmrobFamily = {
			if(activevalue!="quasi")
				data <- cbSetEnableFlag(data, "SPropModelVariance", F)
			else
				data <- cbSetEnableFlag(data, "SPropModelVariance", T)
			switch(activevalue,
				gaussian = {
					data <- cbSetOptionList(data, "SPropModelLink", "identity")
				}
				,
				Gamma = {
					data <- cbSetOptionList(data, "SPropModelLink", "identity,inverse,log")
					data <- cbSetCurrValue(data, "SPropModelLink", "inverse")
				}
				,
				inverse.gaussian = {
					data <- cbSetOptionList(data, "SPropModelLink", "1/mu^2")
				}
				,
			quasi = {
				data <- cbSetOptionList(data, "SPropModelLink", 
                    "identity,log,sqrt,logit,probit,cloglog,inverse,1/mu^2")
				data <- cbSetCurrValue(data, "SPropModelLink", "identity")
				}
				,
			binomial = {
				if(cbGetCurrValue(data, "glmrobMethodBox")=="MLE")
					{
					data <- cbSetOptionList(data, "SPropModelLink", "logit,probit,cloglog")
					data <- cbSetCurrValue(data, "SPropModelLink", "logit")
					}
				else
					{
              data <- cbSetOptionList(data, "SPropModelLink", "logit") 
              data <- cbSetCurrValue(data, "SPropModelLink", "logit")
					}
				}
				,
			poisson = {
				if(cbGetCurrValue(data, "glmrobMethodBox")=="MLE")
					{
					data <- cbSetOptionList(data, "SPropModelLink", "log,sqrt,identity")
					data <- cbSetCurrValue(data, "SPropModelLink", "log")
            		} 
				else
					{
              data <- cbSetOptionList(data, "SPropModelLink", "log")
              data <- cbSetCurrValue(data, "SPropModelLink", "log")
					}
				}
			) # end switch
		} # end SPropGlmrobFamily
		,
		
		glmrobMethodBox = {
			if(activevalue=="MLE")
				{
# enable MLE options
				data <- cbSetEnableFlag(data, "SPropMaxiter", T)
				data <- cbSetEnableFlag(data, "SPropTolerance", T)
				data <- cbSetEnableFlag(data, "SPropTrace", T) 
# disable Options Button
#				data <- cbSetEnableFlag(data, "SPropGlmRobOptButton", F)
# disable ComparePlots
				data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", F)          
				data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", F)
# enable Save Predictions SE
				data <- cbSetEnableFlag(data, "glmrobPredSaveSE", T)
				data <- cbSetOptionList(data, "SPropGlmrobFamily",
                  "gaussian,binomial,poisson,Gamma,inverse.gaussian,quasi")
				
				tmp <- cbGetCurrValue(data, "SPropGlmrobFamily") 
				if(tmp!="quasi")
					data <- cbSetEnableFlag(data, "SPropModelVariance", F)
				else 
					data <- cbSetEnableFlag(data, "SPropModelVariance", T)
				switch(tmp,
					gaussian = {
						data <- cbSetOptionList(data, "SPropModelLink", "identity")
					}
					,
					Gamma = {
						data <- cbSetOptionList(data, "SPropModelLink", "identity,inverse,log")
					}
					,
					inverse.gaussian = {
						data <- cbSetOptionList(data, "SPropModelLink", "1/mu^2")
					}
					,
					quasi = {
						data <- cbSetOptionList(data, "SPropModelLink", 
                      "identity,log,sqrt,logit,probit,cloglog,inverse,1/mu^2")
					}
					,
					binomial = {
						data <- cbSetOptionList(data, "SPropModelLink", "logit,probit,cloglog")
					}
					,
					poisson = {
						data <- cbSetOptionList(data, "SPropModelLink", "log,sqrt,identity")
					}
				) # end switch
			### Disable Advanced Tab Robust Options
			data <- cbSetEnableFlag(data, "glmrobFitMethod", F)
			data <- cbSetEnableFlag(data, "glmrobTolerance", F)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
			data <- cbSetEnableFlag(data, "glmrobCConst", F)
			data <- cbSetEnableFlag(data, "glmrobUfact", F)
			data <- cbSetEnableFlag(data, "glmrobTrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
			data <- cbSetEnableFlag(data, "glmrobMCtol", F)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)

		} # end if(activevalue=="MLE")... 
		
		
		else	
			{ 
# enable Robust estimate options

		data <- cbSetEnableFlag(data, "glmrobFitMethod", T)
		current.method <- cbGetCurrValue(data, "glmrobFitMethod")
			switch(current.method,
				cubif = {
					data <- cbSetEnableFlag(data, "glmrobTolerance", T)
					data <- cbSetEnableFlag(data, "glmrobMaxiter", T)
					data <- cbSetEnableFlag(data, "glmrobCConst", T)
					data <- cbSetEnableFlag(data, "glmrobUfact", T)
					data <- cbSetEnableFlag(data, "glmrobTrace", T)
					data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
					data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
					data <- cbSetEnableFlag(data, "glmrobMCtol", F)
					data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
					data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
					data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
					}
					,
				misclass = {
					data <- cbSetEnableFlag(data, "glmrobTolerance", F)
					data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
					data <- cbSetEnableFlag(data, "glmrobCConst", F)
					data <- cbSetEnableFlag(data, "glmrobUfact", F)
					data <- cbSetEnableFlag(data, "glmrobTrace", F)
					data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
					data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
					data <- cbSetEnableFlag(data, "glmrobMCtrace", T)
					data <- cbSetEnableFlag(data, "glmrobMCmaxit", T)
					data <- cbSetEnableFlag(data, "glmrobMCtol", T)
					data <- cbSetEnableFlag(data, "glmrobMCgamma", T)
					}
					,
				mallows = {
					data <- cbSetEnableFlag(data, "glmrobTolerance", F)
					data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
					data <- cbSetEnableFlag(data, "glmrobCConst", F)
					data <- cbSetEnableFlag(data, "glmrobUfact", F)
					data <- cbSetEnableFlag(data, "glmrobTrace", F)
					data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
					data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
					data <- cbSetEnableFlag(data, "glmrobMCtol", F)
					data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
					data <- cbSetEnableFlag(data, "glmrobMallowsFn", T)
					data <- cbSetEnableFlag(data, "glmrobMallowsTuning", T)
					}
				)

# Enable RR vs RD plot
			data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", T)          
			data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", T)
# Enable Options Button
#			data <- cbSetEnableFlag(data, "SPropGlmRobOptButton", T)
			
          if(activevalue!="Robust") { # activevalue=="MLE + Robust"
# enable MLE options
				data <- cbSetEnableFlag(data, "SPropMaxiter", T)
				data <- cbSetEnableFlag(data, "SPropTolerance", T)
				data <- cbSetEnableFlag(data, "SPropTrace", T) 
# enable Compare Plots
            data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", T) 
			data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", T)
          } else {
# disable MLE options
            data <- cbSetEnableFlag(data, "SPropMaxiter", F)
            data <- cbSetEnableFlag(data, "SPropTolerance", F)
            data <- cbSetEnableFlag(data, "SPropTrace", F) 
# disable Compare Plots	
#            data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", F) 
#				data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", F)
          }  # end if(activevalue!="Robust")
# disable Save Predictions SE
          data <- cbSetEnableFlag(data, "glmrobPredSaveSE", F)
          data <- cbSetOptionList(data, "SPropGlmrobFamily", "binomial,poisson") 
          tmp <- cbGetCurrValue(data, "SPropGlmrobFamily")
          if(is.na(match(tmp, c("binomial","poisson")))) {
            data <- cbSetCurrValue(data, "SPropGlmrobFamily", "binomial")
            data <- cbSetCurrValue(data, "SPropModelLink", "logit")
            data <- cbSetOptionList(data, "SPropModelLink", "logit")
          } else {
            switch(tmp,
              binomial = {
                data <- cbSetOptionList(data, "SPropModelLink", "logit")
              }
              ,
              poisson = {
                data <- cbSetOptionList(data, "SPropModelLink", "log")
              }
            ) # end switch
          } # end if(is.na(match(...)))
          tmp <- cbGetCurrValue(data, "SPropGlmrobFamily")
          if(tmp!="quasi") 
            data <- cbSetEnableFlag(data, "SPropModelVariance", F)
          else 
            data <- cbSetEnableFlag(data, "SPropModelVariance", T)
        } # end if(activevalue=="MLE")
      } # end glmrobMethodBox
      ,
      SPropPrintLong = {
        if(activevalue == "F") 
          data <- cbSetEnableFlag(data, "SPropPrintCor", F)
        else {
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
	   },
      SPropPlotPartialResid = {
        if(activevalue == "F") {
          data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
          data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
          data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
        }
        else {
          data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
          data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
          data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
          data <- cbSetCurrValue( data, "SPropPlotPartialScale", "T")
        }
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
          guiModify("Property", Name="SPropCFFormulaID", DialogControl="Invisible", 
                    DefaultValue=cbGetDialogId(data))
          guiModify("Property", Name="SPropCFData", DialogControl="Invisible", 
                    DefaultValue=cbGetCurrValue(data, "SPropDataFrameList"))
          guiModify("Property", Name="SPropCFFormula", 
                    DefaultValue = cbGetCurrValue(data, "SPropPFFormula"))
          guiDisplayDialog("Function", Name = "makeFormulaGlm")
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
          guiCreate("MessageBox", 
            String = "Data Set doesn't exit. Enter Data Set before building formula.")
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
#		SPropGlmRobOptButton = {
#			guiModify("Property", Name = "SPropGlmRobOptParent",
#					DefaultValue = cbGetDialogId(data))
#			guiDisplayDialog("Function", "menuGlmRobOptions", bModal = T)
#		}
#		,
	glmrobFitMethod = {
		if(activevalue == "cubif") {
			#enable options
			data <- cbSetEnableFlag(data, "glmrobTolerance", T)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", T)
			data <- cbSetEnableFlag(data, "glmrobCConst", T)
			data <- cbSetEnableFlag(data, "glmrobUfact", T)
			data <- cbSetEnableFlag(data, "glmrobTrace", T)
			#disable options
			data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
			data <- cbSetEnableFlag(data, "glmrobMCtol", F)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
		}
		else if(activevalue == "misclass") {
			#enable options
			data <- cbSetEnableFlag(data, "glmrobTolerance", F)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
			data <- cbSetEnableFlag(data, "glmrobCConst", F)
			data <- cbSetEnableFlag(data, "glmrobUfact", F)
			data <- cbSetEnableFlag(data, "glmrobTrace", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
			#disable options
			data <- cbSetEnableFlag(data, "glmrobMCtrace", T)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", T)
			data <- cbSetEnableFlag(data, "glmrobMCtol", T)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", T)
			}
		else
		if(activevalue == "mallows") {
			data <- cbSetEnableFlag(data, "glmrobTolerance", F)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
			data <- cbSetEnableFlag(data, "glmrobCConst", F)
			data <- cbSetEnableFlag(data, "glmrobUfact", F)
			data <- cbSetEnableFlag(data, "glmrobTrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
			data <- cbSetEnableFlag(data, "glmrobMCtol", F)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", T)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", T)
		}
	  }
    )
  }
# end if(cbIsUpdateMessage(...

  if(cbIsRollbackMessage(data)) {
    if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) {
      data <- spropColName(data, "SPropDataFrameList", "SPropDependent")
      list.x <- paste("<ALL>", cbGetOptionList(data, "SPropDependent"), sep = ",")
      data <- cbSetOptionList(data, "SPropIndependent", list.x)
    }
    activevalue <- cbGetCurrValue(data, "SPropGlmrobFamily")
    if(activevalue != "quasi")
      data <- cbSetEnableFlag(data, "SPropModelVariance", F)
    else
      data <- cbSetEnableFlag(data, "SPropModelVariance", T)
    switch(activevalue,
      gaussian = {
        data <- cbSetOptionList(data, "SPropModelLink", "identity")
      }
      ,
      Gamma = {
        data <- cbSetOptionList(data, "SPropModelLink", "identity,inverse,log")
      }
      ,
      inverse.gaussian = {
        data <- cbSetOptionList(data, "SPropModelLink", "1/mu^2")
      }
      ,
      quasi = {
       data <- cbSetOptionList(data, "SPropModelLink", 
               "identity,log,sqrt,logit,probit,cloglog,inverse,1/mu^2")
      }
      ,
      binomial = {
        if(cbGetCurrValue(data, "glmrobMethodBox")=="MLE") {
          data <- cbSetOptionList(data, "SPropModelLink", "logit,probit,cloglog")
        } 
        else {
          data <- cbSetOptionList(data, "SPropModelLink", "logit") 
        } # end if(cbGetCurrValue(...))
      }
      ,
      poisson = {
        if(cbGetCurrValue(data, "glmrobMethodBox")=="MLE") {
          data <- cbSetOptionList(data, "SPropModelLink", "log,sqrt,identity")
        } 
        else {
          data <- cbSetOptionList(data, "SPropModelLink", "log")
        } # end if(cbGetCurrValue(...))
      }
    ) # end switch(activevalue...)
    activevalue <- cbGetCurrValue(data, "SPropPrintLong")
    if(activevalue == "F")
      data <- cbSetEnableFlag(data, "SPropPrintCor", F)
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
      data <- cbSetCurrValue(data, "SPropPrintLong", "F")
    }
    activevalue <- cbGetCurrValue(data, "SPropPlotPartialResid")
    if(activevalue == "F") {
      data <- cbSetEnableFlag(data, "SPropPlotPartialFit", F)
      data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", F)
      data <- cbSetEnableFlag(data, "SPropPlotPartialScale", F)
    } 
    else {
      data <- cbSetEnableFlag(data, "SPropPlotPartialFit", T)
      data <- cbSetEnableFlag(data, "SPropPlotPartialRugplot", T)
      data <- cbSetEnableFlag(data, "SPropPlotPartialScale", T)
      data <- cbSetCurrValue(data, "SPropPlotPartialScale", "T")
    }
	activevalue <- cbGetCurrValue(data, "glmrobMethodBox")
	if( activevalue == "MLE + Robust" ) {
		data <- cbSetEnableFlag(data, "gui_glmrob_overlaid_QQ", T)
		data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", T)
	}
	else {
		if( activevalue == "MLE" ) {
		data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", F)
		data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", F)
		}
			else { 
			data <- cbSetEnableFlag(data, "SPropPlotCompGlmFitsRRvsRD", T)
			data <- cbSetEnableFlag(data, "gui_glmrob_deviance_QQ", T)
		}
	}
    activevalue <- cbGetCurrValue(data, "glmrobFitMethod")
		if(activevalue == "cubif") {
			#enable options
			data <- cbSetEnableFlag(data, "glmrobTolerance", T)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", T)
			data <- cbSetEnableFlag(data, "glmrobCConst", T)
			data <- cbSetEnableFlag(data, "glmrobUfact", T)
			data <- cbSetEnableFlag(data, "glmrobTrace", T)
			#disable options
			data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
			data <- cbSetEnableFlag(data, "glmrobMCtol", F)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
			}
		else
		if(activevalue == "misclass") {
			#enable options
			data <- cbSetEnableFlag(data, "glmrobTolerance", F)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
			data <- cbSetEnableFlag(data, "glmrobCConst", F)
			data <- cbSetEnableFlag(data, "glmrobUfact", F)
			data <- cbSetEnableFlag(data, "glmrobTrace", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", F)
			#disable options
			data <- cbSetEnableFlag(data, "glmrobMCtrace", T)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", T)
			data <- cbSetEnableFlag(data, "glmrobMCtol", T)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", T)
			}
		else
		if(activevalue == "mallows") {
			data <- cbSetEnableFlag(data, "glmrobTolerance", F)
			data <- cbSetEnableFlag(data, "glmrobMaxiter", F)
			data <- cbSetEnableFlag(data, "glmrobCConst", F)
			data <- cbSetEnableFlag(data, "glmrobUfact", F)
			data <- cbSetEnableFlag(data, "glmrobTrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCtrace", F)
			data <- cbSetEnableFlag(data, "glmrobMCmaxit", F)
			data <- cbSetEnableFlag(data, "glmrobMCtol", F)
			data <- cbSetEnableFlag(data, "glmrobMCgamma", F)
			data <- cbSetEnableFlag(data, "glmrobMallowsFn", T)
			data <- cbSetEnableFlag(data, "glmrobMallowsTuning", T)
		}

  } # end if( cbIsRollbackMessage(...
  data
}

#
# end backGlmRob
#
