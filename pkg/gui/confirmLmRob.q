confirmLmRob <- function(yes.p=F)
{
	assign("yes.p", yes.p, frame=1)
	invisible()
}

backConfirmLmRob <- function(data)
{
	initialmsg <- cbIsInitDialogMessage(data)
	activeprop <- cbGetActiveProp(data)
	activevalue <- cbGetCurrValue(data, activeprop)
	if(initialmsg) {
		data <- cbSetCurrValue(data, "SPropRobMsgYes", "F")
		data <- cbSetCurrValue(data, "SPropRobMsgNo",  "T")
	}
	if(cbIsUpdateMessage(data))
		switch(activeprop,
		SPropRobMsgYes={
			if(activevalue == "F") 
				data <- cbSetCurrValue(data, "SPropRobMsgNo", "T")
			else
				data <- cbSetCurrValue(data, "SPropRobMsgNo", "F")
		},
		SPropRobMsgNo={
			if(activevalue == "F") 
				data <- cbSetCurrValue(data, "SPropRobMsgYes", "T")
			else
				data <- cbSetCurrValue(data, "SPropRobMsgYes", "F")
		})
	if(cbIsRollbackMessage(data)) {
		activevalue <- cbGetCurrValue(data, "SPropRobMsgNo")
		if(activevalue == "F") 
			data <- cbSetCurrValue(data, "SPropRobMsgYes", "T")
		else
			data <- cbSetCurrValue(data, "SPropRobMsgYes", "F")
	}
	data
}
