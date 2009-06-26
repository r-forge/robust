##
##	loop tests for lmRob plots
##

	#Global
{
	lmRob.data <- wagner.dat
	lmRob.formula <- y ~ .
	T
}

###########################################################

	#1 test plot.lmRob

{
	#make an lmRob object and start pdf device
	temp <- lmRob(lmRob.formula, data = lmRob.data)
	pdf("plot.lmRob.pdf")
	T
}

{
	#Residuals vs Fitted
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Normal QQplot of Residuals
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#r-f spread plot
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Robust Residuals vs Robust Distances
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Standardized Residuals vs Index (Time)
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Estimated Kernel Density of Residuals
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
	temp <- fit.models(list(Robust = "lmRob", LS = "lm"), lmRob.formula, data = lmRob.data)
	pdf("plot.fit.models.lm.both.pdf")
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
	#Robust Residuals vs Robust Distances
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Standardized Residuals vs Index (Time)
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 9))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:9))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}




############################################################

	#3 Test plot.fit.models with lmRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "lmRob"), lmRob.formula, data = lmRob.data)
	pdf("plot.fit.models.lmRob.only.pdf")
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
	#Robust Residuals vs Robust Distances
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Standardized Residuals vs Index (Time)
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 9))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:9))) != "Error"
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
	temp <- fit.models(list(LS = "lm"), lmRob.formula, data = lmRob.data)
	pdf("plot.fit.models.lm.only.pdf")
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
	#Robust Residuals vs Robust Distances
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#Standardized Residuals vs Index (Time)
	class(try(plot(temp, which = 7))) != "Error"
}

{
	#Overlaid Normal QQ-Plot of Residuals
	class(try(plot(temp, which = 8))) != "Error"
}

{
	#Overlaid Estimated Density of Residuals
	class(try(plot(temp, which = 9))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:9))) != "Error"
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
	rm(lmRob.data)
	rm(lmRob.formula)
	T
}





