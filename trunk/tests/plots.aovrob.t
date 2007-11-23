##
##	loop tests for aovRob plots
##

	#Global
{
	aovRob.data <- lawson.dat
	aovRob.formula <- y ~ .
	T
}

###########################################################

	#1 test plot.lmRob

{
	#make an lmRob object and start pdf device
	temp <- aovRob(aovRob.formula, data = aovRob.data)
	pdf.graph("plot.aovRob.pdf")
	T
}

{
	#Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Estimated Kernel Density of Residuals
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Residual-Fit Spread
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:8))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	T
}


################################################################

	#2 Test plot.fit.models with lmRob comparison

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "aovRob", LS = "aov"), aovRob.formula, data = aovRob.data)
	pdf.graph("plot.fit.models.aov.both.pdf")
	T
}

{
	#Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Estimated Kernel Density of Residuals
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Residual-Fit Spread
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:8))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}




############################################################

	#3 Test plot.fit.models with aovRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "aovRob"), aovRob.formula, data = aovRob.data)
	pdf.graph("plot.fit.models.aovRob.only.pdf")
	T
}

{
	#Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Estimated Kernel Density of Residuals
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Residual-Fit Spread
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:8))) != "Error"
}


{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}

#################################################################

	#4 Test plot.fit.models with lm only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(LS = "aov"), aovRob.formula, data = aovRob.data)
	pdf.graph("plot.fit.models.aov.only.pdf")
	T
}

{
	#Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Estimated Kernel Density of Residuals
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Residual-Fit Spread
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:8))) != "Error"
}


{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}

#####################################################



	# Remove Globals
{
	rm(aovRob.data)
	rm(aovRob.formula)
	T
}





