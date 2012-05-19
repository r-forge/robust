

menuDiscRob <- function(formula, data, weights, frequencies,
               subset, naOmit = T, family, cov.structure, prior,
               printShort, printLong = T, plugIn = F, predictive = F, 
               unbiased = F, crossValidate = F, predResult, 
               doPlots=F, fitting.method = "Robust", method="MCD", 
							 quan="Auto", trial=500, autoOne = "Auto", autoTwo = "Auto",
							 random.sample = F, strOne = .45, strTwo = .05,
							 strThree = 1e-006, strFour = .001, rob.int = 150)
{
	warn <- options("warn")
	on.exit(options(warn))

	control.call <- call("covRob.control")
	switch(method,
		"Donoho-Stahel" = {
			control.call$estim <- "donostah"
			method <- "donostah"
			if(autoOne != "Auto")
			{
				if(autoOne == "All")
					control.call$nresamp <- 0
				else
					control.call$nresamp <- as.integer(autoOne)
			}
			if(autoTwo != "Auto")
				control.call$maxres <- as.integer(autoTwo)
			if(random.sample)
				control.call$random.sample <- T
			if(strOne != .95)
				control.call$tune <- strOne
			if(strTwo != .99)
				control.call$prob <- strTwo
			if(strThree != .5)
				control.call$eps <- strThree
		},
		
		"M" = {
			control.call$estim <- "M"
			if(quan != "Auto")
				control.call$quan <- as.numeric(quan)
			if(trial != 500)
				control.call$ntrial <- trial
			if(strOne != .45)
				control.call$r <- strOne
			if(strTwo != .05)
				control.call$alpha <- strTwo
			if(rob.int != 150)
				control.call$maxit <- rob.int
			if(strThree != 1e-006)
				control.call$tau <- strThree
			if(strFour != .001)
				control.call$tol <- strFour
		},
		
		"MCD" = {
			control.call$estim <- "MCD"
			if(quan != "Auto")
				control.call$quan <- as.numeric(quan)
			if(trial != 500)
				control.call$ntrial <- trial
		}
		)

	switch(family,
		"principal component" = family <- as.name("CPC"),
		classical = family <- as.name("Classical"),
		canonical = family <- as.name("Canonical")
	)
	
	family <- call(family)
	family[[2]] <- cov.structure

	m <- match.call()
	m$method <- m$quan <- m$trial <- m$autoOne <- m$autoTwo <- NULL
	m$random.sample <- m$strOne <- m$strTwo <- m$strThree <- NULL
	m$strFour <- m$rob.int <- NULL

	if(fitting.method == "MLE") {
		m[[1]] <- as.name("discrim")
		m$naOmit <- m$cov.structure <- m$printShort <- m$printLong <- m$plugIn <- NULL
		m$predictive <- m$unbiased <- m$crossValidate <- m$predResult <- m$doPlots <- NULL
		if(!missing(data))
			m$data <- substitute(data)
		if(!missing(weights))
			m$weights <- substitute(weights)
		if(!missing(frequencies))
			m$frequencies <- substitute(frequencies)
		if(!missing(prior)) {
			if((k <- pmatch(prior,c("proportional","uniform","none"), nomatch = 0)))
				m$prior <- prior
			else 
				if(parse.test(prior) == "incomplete")
					m$prior <- eval(parse(text = prior))
				else 
					if(parse.test(paste("c(", prior, ")", sep = " ")) == "incomplete")
						m$prior <- eval(parse(text = paste("c(", prior, ")", sep = " ")))
					else stop(paste("prior", prior, "is invalid"))						
		}	
		if(!missing(subset))
			m$subset <- substitute(subset)
		m[["family"]] <- family
	   if(naOmit)
			m$na.action <- as.name("na.omit")
		fit <- eval(m)
		predMethod <- c("plug-in", "predictive", "unbiased")[c(plugIn,
	                predictive, unbiased)]
		if(printShort || printLong) {
			cat("\n\t*** Robust Discriminant Analysis ***\n")
			if(printShort) {
				print(fit)
			}	
			if(printLong){
				if(length(predMethod) == 0)
					print(summary(fit))
				else print(summary(fit, method = predMethod[1]))	
			}
			cat("\n")
		}	
		if (crossValidate)
			predMethod <- c(predMethod, "CV")
		if (length(predMethod) > 0) {
			bShow <- F
			for(predMeth in predMethod) {
				if(predMeth == "CV")
					x <- crossvalidate(fit)
				else x <- predict(fit, method = predMeth)
			names(x) <- paste(abbreviate(make.names(predMeth), 4), 
			    abbreviate(names(x), 8), sep = ".")
			if(!is.all.white(predResult)) {
				if(exists(predResult)) {
					if(!inherits(get(predResult), "data.frame")) {
						guiCreate("MessageBox", String = paste("Object",
						          predResult, "must be derived from the 
						          data.frame class.", "A new object will 
						          be created."))
						predResult <- " "
					}
					else if (dim(x)[1] != dim(get(predResult))[1]) {
						assign(predResult, cbind(guiAsDefaultDataObject(
						       get(predResult)), x), where = 1,
						       immediate = T)	
					}
					else {
						assign(predResult, cbind(get(predResult), x),
						where = 1, immediate = T)
					}	
				}
				else assign(predResult, x, where = 1, immediate = T)
			}	
				
			if (is.all.white(predResult)) {
				k <- 1
				predResult <- "DS1"
				while(exists(predResult) && (k <- k+1) <= 500) 
						predResult <- paste("DS", k, sep = " ")
				if(k > 500) {
						warning("could not generate unique data set name,
						        'DSx', for predicted values")
						predResult <- " "
						break
				}
				bShow <- is.ui.app("any")
				assign(predResult, x, where = 1, immediate = T)	 
			}
		}
		if(is.ui.app("s+gui") && bShow && exists(predResult))
			guiOpenView(classname = data.class(get(predResult)), 
			            Name = predResult)
	}					
	if(doPlots)
		plot(fit)
	return(invisible(fit))		
}

else if( fitting.method == "Robust" ) {
		m[[1]] <- as.name("discRob")
		m$naOmit <- m$cov.structure <- m$printShort <- m$printLong <- m$plugIn <- NULL
		m$predictive <- m$unbiased <- m$crossValidate <- m$predResult <- m$doPlots <- NULL
		if(!missing(data))
			m$data <- substitute(data)
		if(method != "Auto")
			m$estim <- method
		if(!missing(weights))
			m$weights <- substitute(weights)
		if(!missing(frequencies))
			m$frequencies <- substitute(frequencies)
		if(!missing(prior)) {
			if((k <- pmatch(prior,c("proportional","uniform","none"), nomatch = 0)))
				m$prior <- prior
			else 
				if(parse.test(prior) == "incomplete")
					m$prior <- eval(parse(text = prior))
				else 
					if(parse.test(paste("c(", prior, ")", sep = " ")) == "incomplete")
						m$prior <- eval(parse(text = paste("c(", prior, ")", sep = " ")))
					else stop(paste("prior", prior, "is invalid"))						
		}	
		if(!missing(subset))
			m$subset <- substitute(subset)
		m[["family"]] <- family
	   if(naOmit)
			m$na.action <- as.name("na.omit")

		if(length(control.call) > 2)
			m$cov.control = substitute(control.call)

		fit <- eval(m)
		predMethod <- c("plug-in", "predictive", "unbiased")[c(plugIn,
	                predictive, unbiased)]
		if(printShort || printLong) {
			cat("\n\t*** Robust Discriminant Analysis ***\n")
			if(printShort) {
				print(fit)
			}	
			if(printLong){
				if(length(predMethod) == 0)
					print(summary(fit))
				else print(summary(fit, method = predMethod[1]))	
			}
			cat("\n")
		}	
		if (crossValidate)
			predMethod <- c(predMethod, "CV")
		if (length(predMethod) > 0) {
			bShow <- F
			for(predMeth in predMethod) {
				if(predMeth == "CV")
					x <- crossvalidate(fit)
				else x <- predict(fit, method = predMeth)
			names(x) <- paste(abbreviate(make.names(predMeth), 4), 
			    abbreviate(names(x), 8), sep = ".")
			if(!is.all.white(predResult)) {
				if(exists(predResult)) {
					if(!inherits(get(predResult), "data.frame")) {
						guiCreate("MessageBox", String = paste("Object",
						          predResult, "must be derived from the 
						          data.frame class.", "A new object will 
						          be created."))
						predResult <- " "
					}
					else if (dim(x)[1] != dim(get(predResult))[1]) {
						assign(predResult, cbind(guiAsDefaultDataObject(
						       get(predResult)), x), where = 1,
						       immediate = T)	
					}
					else {
						assign(predResult, cbind(get(predResult), x),
						where = 1, immediate = T)
					}	
				}
				else assign(predResult, x, where = 1, immediate = T)
			}	
				
			if (is.all.white(predResult)) {
				k <- 1
				predResult <- "DS1"
				while(exists(predResult) && (k <- k+1) <= 500) 
						predResult <- paste("DS", k, sep = " ")
				if(k > 500) {
						warning("could not generate unique data set name,
						        'DSx', for predicted values")
						predResult <- " "
						break
				}
				bShow <- is.ui.app("any")
				assign(predResult, x, where = 1, immediate = T)	 
			}
		}
		if(is.ui.app("s+gui") && bShow && exists(predResult))
			guiOpenView(classname = data.class(get(predResult)), 
			            Name = predResult)
	}					
	if(doPlots)
		plot(fit)
	return(invisible(fit))		
}

else
	{ 		### fitting.method equals "MLE + Robust" ###
		
		
		m[[1]] <- as.name("fit.models")
		fun.args <- is.element(arg.names(m), arg.names(fit.models))
		m <- m[c(T, fun.args)]
		m$naOmit <- m$cov.structure <- m$printShort <- m$printLong <- m$plugIn <- NULL
		m$predictive <- m$unbiased <- m$crossValidate <- m$predResult <- m$doPlots <- NULL
		m[["model"]] <- list(MLE = "discrim", Robust = "discRob")
		if(!missing(data))
			m$data <- substitute(data)
		if(!missing(weights))
			m$weights <- substitute(weights)
		if(!missing(frequencies))
			m$frequencies <- substitute(frequencies)
		if(method != "Auto")
			m$estim <- method
		if(length(control.call) > 2)
			m$cov.control <- substitute(control.call)
		if(!missing(prior)) {
			if((k <- pmatch(prior,c("proportional","uniform","none"), nomatch = 0)))
				m$prior <- prior
			else 
				if(parse.test(prior) == "incomplete")
					m$prior <- eval(parse(text = prior))
				else 
					if(parse.test(paste("c(", prior, ")", sep = " ")) == "incomplete")
						m$prior <- eval(parse(text = paste("c(", prior, ")", sep = " ")))
					else stop(paste("prior", prior, "is invalid"))						
		}	
		if(!missing(subset))
			m$subset <- substitute(subset)
		m[["family"]] <- family
	   if(naOmit)
			m$na.action <- as.name("na.omit")
		fit <- eval(m)
		predMethod <- c("plug-in", "predictive", "unbiased")[c(plugIn,
	                predictive, unbiased)]
		if(printShort || printLong) {
			cat("\n\t*** Robust Discriminant Analysis ***\n")
			if(printShort) {
				print(fit)
			}	
			if(printLong){
				if(length(predMethod) == 0)
					print(summary(fit))
				else print(summary(fit, method = predMethod[1]))	
			}
			cat("\n")
		}	
		if (crossValidate)
			predMethod <- c(predMethod, "CV")
		if (length(predMethod) > 0) {
			bShow <- F
			for(predMeth in predMethod) {
				if(predMeth == "CV")
					x <- crossvalidate(fit)
				else x <- predict(fit, method = predMeth)
			names(x) <- paste(abbreviate(make.names(predMeth), 4), 
			    abbreviate(names(x), 8), sep = ".")
			if(!is.all.white(predResult)) {
				if(exists(predResult)) {
					if(!inherits(get(predResult), "data.frame")) {
						guiCreate("MessageBox", String = paste("Object",
						          predResult, "must be derived from the 
						          data.frame class.", "A new object will 
						          be created."))
						predResult <- " "
					}
					else if (dim(x)[1] != dim(get(predResult))[1]) {
						assign(predResult, cbind(guiAsDefaultDataObject(
						       get(predResult)), x), where = 1,
						       immediate = T)	
					}
					else {
						assign(predResult, cbind(get(predResult), x),
						where = 1, immediate = T)
					}	
				}
				else assign(predResult, x, where = 1, immediate = T)
			}	
				
			if (is.all.white(predResult)) {
				k <- 1
				predResult <- "DS1"
				while(exists(predResult) && (k <- k+1) <= 500) 
						predResult <- paste("DS", k, sep = " ")
				if(k > 500) {
						warning("could not generate unique data set name,
						        'DSx', for predicted values")
						predResult <- " "
						break
				}
				bShow <- is.ui.app("any")
				assign(predResult, x, where = 1, immediate = T)	 
			}
		}
		if(is.ui.app("s+gui") && bShow && exists(predResult))
			guiOpenView(classname = data.class(get(predResult)), 
			            Name = predResult)
	}					
	if(doPlots)
		plot(fit)
	return(invisible(fit))		
	}
	
		
		
				
} # end menuRobDisc()

