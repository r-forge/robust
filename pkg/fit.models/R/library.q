.onAttach <- function(libname, pkgname)
{
  assign("fmreg", list(), pos = fit.models:::fm.registry)
  

  fmclass.register(fmclass = "lmfm",
                   classes = c("lm"),
                   validation.function = NULL,
                   attributes.function = NULL)
                                 
  fmclass.register(fmclass = "glmfm",
                   classes = c("glm", "lm"),
                   validation.function = NULL,
                   attributes.function = NULL)

  invisible()
}


