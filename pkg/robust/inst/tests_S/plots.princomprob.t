##
##	loop tests for princompRob plots
##

	#Global
{
	data(wood, package = "robustbase")
	princompRob.data <- wood[, 1:5]
	T
}

###########################################################

	#1 test plot.princompRob

{
	#make a princompRob object and start pdf device
	temp <- princompRob(data = princompRob.data, control = covRob.control(estim = "donostah"))
	pdf("plot.princompRob.pdf")
	T
}

{
	#try plot.princompRob
	class(try(plot(temp))) != "Error"
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	T
}


################################################################

	#2 Test plot.fit.models with princompRob comparison
{
	require("fit.models")
}

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "princompRob", Classical = "princomp"), data = princompRob.data, control = covRob.control(estim = "donostah"))
	pdf("plot.fit.models.princomp.both.pdf")
	T
}

{
	#Trellis of Component Scatter Plots: 2 models
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Loadings: 2 models
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Variances: 2 models
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#All: 2 models
	class(try(plot(temp, which = 1:3))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}




############################################################

	#3 Test plot.fit.models with princompRob only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "princompRob"), data = princompRob.data, control = covRob.control(estim = "donostah"))
	pdf("plot.fit.models.princompRob.only.pdf")
	T
}

{
	#Trellis of Component Scatter Plots: Robust model
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Loadings: Robust model
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Variances: Robust model
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#All: Robust model
	class(try(plot(temp, which = 1:3))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp)
	T
}

#################################################################

	#4 Test plot.fit.models with princomp only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Classical = "princomp"), data = princompRob.data, control = covRob.control(estim = "donostah"))
	pdf("plot.fit.models.princomp.only.pdf")
	T
}

{
	#Trellis of Component Scatter Plots: Classical model
	class(try(plot(temp, which = 1))) != "Error"
}

{
	#Loadings: Classical model
	class(try(plot(temp, which = 2))) != "Error"
}

{
	#Variances: Classical model
	class(try(plot(temp, which = 3))) != "Error"
}

{
	#All: Classical model
	class(try(plot(temp, which = 1:3))) != "Error"
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
	rm(princompRob.data)
	T
}





