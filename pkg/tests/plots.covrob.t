##
##	loop tests for covRob plots
##

	#Global
{
	covRob.data <- woodmod.dat
	T
}

###########################################################

	#1 test plot.covRob

{
	#make a covRob object and start pdf device
	temp <- covRob(data = covRob.data, control = covRob.control(estim = "donostah"))
	pdf.graph("plot.covRob.pdf")
	T
}

{
	#Eigenvalues of Covariance Estimate
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of Mahalanobis Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Ellipses Matrix
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:3))) != "Error"
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
	temp <- fit.models(list(Robust = "covRob", Classical = "cov"), data = covRob.data, control = covRob.control(estim = "donostah"))
	pdf.graph("plot.fit.models.cov.both.pdf")
	T
}

{
	#Eigenvalues of Covariance Estimate
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of Mahalanobis Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Distance - Distance Plot
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#Ellipses Matrix
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#All
	class(try(plot(temp, which = 1:4))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}




############################################################

	#3 Test plot.fit.models with covRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "covRob"), data = covRob.data, control = covRob.control(estim = "donostah"))
	pdf.graph("plot.fit.models.covRob.only.pdf")
	T
}

{
	#Eigenvalues of Covariance Estimate
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of Mahalanobis Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Ellipses Matrix
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#All
	class(try(plot(temp, which = c(1,2,4)))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}

#################################################################

	#4 Test plot.fit.models with cov only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Classical = "cov"), data = covRob.data, control = covRob.control(estim = "donostah"))
	pdf.graph("plot.fit.models.cov.only.pdf")
	T
}

{
	#Eigenvalues of Covariance Estimate
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Sqrt of Mahalanobis Distances
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Ellipses Matrix
	class(try(plot(temp, which = 4))) != "Error"
}

{
	#All
	class(try(plot(temp, which = c(1,2,4)))) != "Error"
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
	rm(covRob.data)
	T
}





