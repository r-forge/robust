test.sourcer <- function(lib.loc = "/a/homer/users/kkonis")
{
	do.test(paste(lib.loc, "robust/tests/lmrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/covm.t", sep = "/"))            
	do.test(paste(lib.loc, "robust/tests/discrob.t", sep = "/"))       
	do.test(paste(lib.loc, "robust/tests/gamrob.t", sep = "/"))      
	do.test(paste(lib.loc, "robust/tests/glmrob.t", sep = "/"))  
	do.test(paste(lib.loc, "robust/tests/lmrob.t", sep = "/")) 
	do.test(paste(lib.loc, "robust/tests/princomprob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/wblrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.aovrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.covrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.glmrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.lmrob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.princomprob.t", sep = "/"))
	do.test(paste(lib.loc, "robust/tests/plots.wblrob.t", sep = "/"))

	"finished"
}

