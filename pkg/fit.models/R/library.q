.onAttach <- function(libname, pkgname)
{
  fm.register.vclass(vclass = "lmfm",
                     classes = c("lm"),
                     validation.function = NULL,
                     attributes.function = NULL)
                                 
  fm.register.vclass(vclass = "glmfm",
                     classes = c("glm"),
                     validation.function = NULL,
                     attributes.function = NULL)

  invisible()
}


