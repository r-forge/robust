backPrinCompRob <- function(data)
{
	activeprop <- cbGetActiveProp(data)
	activevalue <- cbGetCurrValue(data, activeprop)

	if(cbIsInitDialogMessage(data)) {
		data <- cbSetOptionList(data, "SPropNAMethod", "omit, fail")
		data <- cbSetCurrValue(data, "SPropPrintShort", T)
		data <- cbSetCurrValue(data, "SPropPrintImportance", T)
		data <- cbSetCurrValue(data, "SPropPrintLoadings", F)
		data <- cbSetCurrValue(data, "SPropLoadOptions", 0.1)
		data <- cbSetCurrValue(data, "SPropLoadOptions", 0.1)
		data <- cbSetEnableFlag(data, "SPropLoadOptions", F)
		data <- cbSetCurrValue(data, "SPropPrinCompRobSP", F)
		data <- cbSetEnableFlag(data, "SPropPrinCompRobSPcomps", F)

		if(exists(data.name <- cbGetCurrValue(data, "SPropDataX2"))) {
			data <- cbSetOptionList(data, "SPropVariableX2", 
				cbGetColumnNamesString(data.name,prepend="<ALL>"))
			data <- cbSetCurrValue(data, "SPropVariableX2",
				guiGetSelectedCols(data.name, default="<ALL>"))
		}
		else {
			data <- cbSetOptionList(data, "SPropVariableX2", "")
			data <- cbSetCurrValue(data, "SPropVariableX2", "")
		}

		data <- cbSetCurrValue(data, "SPropCovRobCor", "Covariances")
		data <- cbSetCurrValue(data, "SPropCovRobRad", "Both")
		data <- cbSetCurrValue(data, "SPropCovRobPlt", "T")
		data <- cbSetCurrValue(data, "SPropCovRobDst", "T")
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

	if (cbIsUpdateMessage(data)) 
		switch(activeprop,
		SPropDataX2 = {
			if(exists(data.name <- cbGetCurrValue(data, "SPropDataX2"))) {
				data <- cbSetOptionList(data, "SPropVariableX2", 
					cbGetColumnNamesString(data.name,prepend="<ALL>"))
				data <- cbSetCurrValue(data, "SPropVariableX2",
					guiGetSelectedCols(data.name, default="<ALL>"))
			}
			else {
				data <- cbSetOptionList(data, "SPropVariableX2", "")
				data <- cbSetCurrValue(data, "SPropVariableX2", "")
			}
		}
		,
		SPropCovRobRad = {
			if (activevalue == "Classical") {
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
				data <- cbSetCurrValue(data, "SPropCovRobImage", F)
				data <- cbSetEnableFlag(data, "SPropCovRobImage", F)
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

			if (activevalue == "Robust") {
				data <- cbSetCurrValue(data, "SPropCovRobImage", F)
				data <- cbSetEnableFlag(data, "SPropCovRobImage", F)
			}

			if (activevalue == "Both") {
				data <- cbSetCurrValue(data, "SPropCovRobImage", T)
			}
		}
		,
		SPropPrintLoadings = {
			if (as.logical(activevalue) == T){
				data <- cbSetEnableFlag(data, "SPropLoadOptions", T)
			}
			else {
				data <- cbSetEnableFlag(data, "SPropLoadOptions", F)
			}
		}
		,
		SPropPrinCompRobSP = {
			if (as.logical(activevalue) == F){
				data <- cbSetEnableFlag(data, "SPropPrinCompRobSPcomps", F)
			}
			else {
				data <- cbSetEnableFlag(data, "SPropPrinCompRobSPcomps", T)
			}
		}
		,
		SPropCovRobMCD = {
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
		
	if (cbIsRollbackMessage(data)) {
		if(exists(data.name <- cbGetCurrValue(data, "SPropDataX2"))) {
			data <- cbSetOptionList(data, "SPropVariableX2", 
				cbGetColumnNamesString(data.name,prepend="<ALL>"))
		}
		else {
			data <- cbSetOptionList(data, "SPropVariableX2", "")
			data <- cbSetCurrValue(data, "SPropVariableX2", "")
		}
		activevalue <- cbGetCurrValue(data, "SPropCovRobRad")
		if (activevalue == "Classical") {
			data <- cbSetEnableFlag(data, "SPropRobButton", F)
		}
		else {
			data <- cbSetEnableFlag(data, "SPropRobButton", T)
		}
	}
	invisible(data)
}







