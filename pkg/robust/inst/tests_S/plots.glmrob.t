#### -*- R -*-

##
## Tests for glmRob and fit.models(<glm> .) plots
##           ------------------------------------

## Global
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
    ## Each of the 7 kinds of plots  which =  2 .. 7 :
    all(sapply(2:7, function(wh)
               class(try(plot(temp, which = wh))) != "Error"))
}

{
    ## All
    all(class(try(plot(temp, which = "all"))) != "try-error")
}

{
	#clean up and write to file
	rm(temp)
	dev.off()
	TRUE
}


################################################################

## 2 Test plot.fit.models with glmRob - glm  Comparison
{
	require("fit.models")
}

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(Robust = "glmRob", MLE = "glm"),
                           glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.fit.models.glm.both.pdf")
	TRUE
}

{
    ## Each of the 7 kinds of plots  which =  2 .. 7 :
    all(sapply(2:7, function(wh)
               class(try(plot(temp, which = wh))) != "try-error"))
}

{
    ## All
    class(try(plot(temp, which = "all"))) != "try-error"
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
	temp <- fit.models(list(Robust = "glmRob"),
                           glmRob.formula, data = breslow.dat, family = "poisson")
        st <- summary(temp)
        print(st) # gave error, now ok
	pdf("plot.fit.models.glmRob.only.pdf")
	TRUE
}

{
    ## Each of the 7 kinds of plots  which =  2 .. 7 :
    all(sapply(2:7, function(wh)
               class(try(plot(temp, which = wh))) != "Error"))
}

{
    ## All
    class(try(plot(temp, which = "all"))) != "Error"
}

{
	#clean up and write to file
	dev.off()
	rm(temp, st)
	TRUE
}

#################################################################

	#4 Test plot.fit.models with glm only

{
	#make a fit.models object and start pdf device
	temp <- fit.models(list(MLE = "glm"), glmRob.formula, data = breslow.dat, family = "poisson")
	pdf("plot.fit.models.glm.only.pdf")
	TRUE
}

{
    ## Each of the 7 kinds of plots  which =  2 .. 7 :
    all(sapply(2:7, function(wh)
               class(try(plot(temp, which = wh))) != "Error"))
}

{
    ## All
    class(try(plot(temp, which = "all"))) != "Error"
}


#################################################################

{
	#clean up and write to file
	dev.off()
	rm(temp, glmRob.formula, breslow.dat)
	TRUE
}
