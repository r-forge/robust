#### -*- R -*-

##
##	loop tests for glmRob plots
##

	#Global
{
	data(breslow.dat)
	glmRob.formula <- sumY ~ Age10 + Base4 * Trt
	TRUE
}

###########################################################

	#1 test plot.glmRob

{
	#make an glmRob object and start pdf device
	temp <- glmRob(glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.glmRob.pdf")
	TRUE
}

{
	#Residuals vs Fitted Values
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of abs(Residuals) vs Predictions
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Normal QQplot of Pearson Residuals
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#RR vs RD
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#QQplot of Deviance Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:6))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	TRUE
}


################################################################

	#2 Test plot.fit.models with glmRob comparison

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "glmRob", MLE = "glm"), glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.fit.models.glm.both.pdf")
	TRUE
}

{
	#Normal QQ-Plot of Pearson Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Standardized Pearson Residuals vs Robust Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Deviance Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Deviance Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#QQ-Plot of Deviance Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:6))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	TRUE
}




############################################################

	#3 Test plot.fit.models with glmRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "glmRob"), glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.fit.models.glmRob.only.pdf")
	TRUE
}

{
	#Normal QQ-Plot of Pearson Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Standardized Pearson Residuals vs Robust Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Deviance Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Deviance Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#QQ-Plot of Deviance Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:6))) != "Error"
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
	temp <- fit.models(list(MLE = "glm"), glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.fit.models.glm.only.pdf")
	TRUE
}
{
	#Normal QQ-Plot of Pearson Residuals
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Standardized Pearson Residuals vs Robust Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Deviance Residuals vs Fitted Values
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Sqrt of abs(Deviance Residuals) vs Fitted Values
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#Response vs Fitted Values
	class(try(plot(temp, which = 5))) != "Error"
}

{
	#QQ-Plot of Deviance Residuals
	class(try(plot(temp, which = 6))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:6))) != "Error"
}


#################################################################

{
	#clean up and write to file
	dev.off()
	rm(temp, glmRob.formula, breslow.dat)
	TRUE
}
