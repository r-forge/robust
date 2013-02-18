.onLoad <- function(libname, pkgname)
{
  library.dynam("robust", package = pkgname, lib.loc = libname)
  invisible()
}


.onAttach <- function(libname, pkgname)
{
  fmclass.add.class("lmfm", "lmRob")
  fmclass.add.class("glmfm", "glmRob")

  fmclass.register("covfm", c("covRob", "covMLE"))
  fmclass.register("fdfm", c("fitdstnRob", "fitdstn"))

  invisible()
}



