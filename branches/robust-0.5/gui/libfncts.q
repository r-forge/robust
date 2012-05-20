.First.lib <- function(lib.loc, section)
{

	if(existsFunction("guiCreate") && interactive()) {

	if ( is.loaded("s_guiAreCustomMenusEnabled") ) {
		if ( .Call("s_guiAreCustomMenusEnabled") ) {

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust",
			Type = "Menu",
			MenuItemText = "&Robust",
			Index = 14,
			Overwrite = F)

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$Lm",
			Type = "MenuItem",
			MenuItemText = "&Linear Model ...",
			Action = "Function", 
			Command = "menuLmRob")

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$GlmRob",
			Type = "MenuItem", 
			MenuItemText = "&Generalized Linear Models ...",
			Action = "Function", 
			Command = "menuGlmRob")

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$AovRob",
			Type = "MenuItem",
			MenuItemText = "&Fixed Effects ANOVA ...",
			Action = "Function",
			Command = "menuAovRob")

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$CovRob",
			Type = "MenuItem", 
			MenuItemText = "&Covariance (Correlation) ...",
			Action = "Function", 
			Command = "menuCovRob") 

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$PrinCompRob",
			Type = "MenuItem", 
			MenuItemText = "&Principal Components ...",
			Action = "Function",
			Command = "menuPrinCompRob")

		guiCreate("MenuItem", Name = "SPlusMenuBar$Robust$MenuDiscRob",
			Type = "MenuItem",
			Action = "Function",
			Command = "menuDiscRob",
			MenuItemText = "&Discriminant Analysis ...")

		guiCreate("MenuItem",
			Name = "SPlusMenuBar$Robust$gamRob",
			Type = "MenuItem",
			Action = "Function",
			Command = "menuGamRob",
			MenuItemText = "&Asymmetric Parameter Estimation ...")

		}
	}

		guiLoadDefaultObjects("Property",
			FileName = paste(lib.loc, section, "robprop.dft", sep = "/"))

		guiLoadDefaultObjects("FunctionInfo",
			FileName = paste(lib.loc, section, "robfunc.dft", sep = "/"))
	}

	if(search()[2] != "robust") {
		cat("The Robust Library should be loaded at the beginning of the search\n",
			"list using library(robust, first=T), or by checking the appropriate\n",
			"box in the File | Load Library dialog box.\n\n")
	}

	invisible()
}

.Last.lib <- function(lib.loc, section, .data, where)
{
	if(existsFunction("guiRemove") && interactive()) {

	if ( is.loaded("s_guiAreCustomMenusEnabled") ) {
		if ( .Call("s_guiAreCustomMenusEnabled") ) {

		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$Lm")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$GlmRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$AovRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$CovRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$PrinCompRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$gamRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust$MenuDiscRob")
		guiRemove("MenuItem", Name="SPlusMenuBar$Robust")

		}
	}

	}
	invisible()	
}


