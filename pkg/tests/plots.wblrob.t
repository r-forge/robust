##
##	loop tests for covRob plots
##

	#Global
{
	the.data <- los
	T
}

###########################################################

	#1 test plot.gammaRob

{
	#make a gammaRob object and start pdf device
	temp <- gammaRob(data = the.data)
	pdf.graph("plot.gamma.pdf")
	T
}

{
	#plot.gammaRob
	class(try(plot(temp))) != "Error"
}

{
	#make a gammaMLE object
	temp <- gammaMLE(data = the.data)
	T
}

{
	#plot.gammaMLE
	class(try(plot(temp))) != "Error"
}

{
	#make a gamma fit.models object
	temp <- fit.models(list(Robust = "gammaRob", MLE = "gammaMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a gamma fit.models object with only robust model
	temp <- fit.models(list(Robust = "gammaRob"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a gamma fit.models object with only MLE model
	temp <- fit.models(list(MLE = "gammaMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	T
}

#################################################################

	#1 test plot.weibullRob

{
	#make a weibullRob object and start pdf device
	temp <- weibullRob(data = the.data)
	pdf.graph("plot.weibull.pdf")
	T
}

{
	#plot.weibullRob
	class(try(plot(temp))) != "Error"
}

{
	#make a weibullMLE object
	temp <- weibullMLE(data = the.data)
	T
}

{
	#plot.weibullMLE
	class(try(plot(temp))) != "Error"
}

{
	#make a weibull fit.models object
	temp <- fit.models(list(Robust = "weibullRob", MLE = "weibullMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a weibull fit.models object with only robust model
	temp <- fit.models(list(Robust = "weibullRob"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a weibull fit.models object with only MLE model
	temp <- fit.models(list(MLE = "weibullMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	T
}

#################################################################

	#1 test plot.lognormRob

{
	#make a lognormRob object and start pdf device
	temp <- lognormRob(data = the.data)
	pdf.graph("plot.lognorm.pdf")
	T
}

{
	#plot.lognormRob
	class(try(plot(temp))) != "Error"
}

{
	#make a lognormMLE object
	temp <- lognormMLE(data = the.data)
	T
}

{
	#plot.lognormMLE
	class(try(plot(temp))) != "Error"
}

{
	#make a lognorm fit.models object
	temp <- fit.models(list(Robust = "lognormRob", MLE = "lognormMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a lognorm fit.models object with only robust model
	temp <- fit.models(list(Robust = "lognormRob"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#make a lognorm fit.models object with only MLE model
	temp <- fit.models(list(MLE = "lognormMLE"), data = the.data)
	T
}

{
	#Overlaid Density Estimates
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Response vs Estimated Quantiles
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	T
}

#################################################################


	#Clean Global
{
	rm(the.data)
	T
}
