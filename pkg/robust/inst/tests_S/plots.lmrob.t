#### -*- R -*-
####
####	loop tests for lmRob plots
####

## Global
{
    source(system.file("datasets", "wagner.q",
		       package = "robust")) # wagnerGrowth
    is.data.frame(lmRob.data <- wagnerGrowth)
}

###########################################################

	#1 test plot.lmRob

{
	#make an lmRob object and start pdf device
	temp <- lmRob(y ~ ., data = lmRob.data)
	pdf("plot.lmRob.pdf")
	TRUE
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
	TRUE
}


################################################################

	#2 Test plot.fit.models with lmRob comparison
{
	require("fit.models")
}


{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "lmRob", LS = "lm"), y ~ ., data = lmRob.data)
	pdf("plot.fit.models.lm.both.pdf")
	TRUE
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
	TRUE
}




############################################################

	#3 Test plot.fit.models with lmRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "lmRob"), y ~ ., data = lmRob.data)
	pdf("plot.fit.models.lmRob.only.pdf")
	TRUE
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
	TRUE
}

#################################################################

	#4 Test plot.fit.models with lm only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(LS = "lm"), y ~ ., data = lmRob.data)
	pdf("plot.fit.models.lm.only.pdf")
	TRUE
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
	TRUE
}

#####################################################



	# Remove Globals
{
	rm(lmRob.data)
	TRUE
}





