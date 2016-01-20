.onLoad <- function(libname, pkgname)
{
  library.dynam("robust", package = pkgname, lib.loc = libname)

  ##--------------- begin {fit.models} -----------------
  requireNamespace("fit.models")
  FM.add.class <- fit.models::fmclass.add.class
  FM.register  <- fit.models::fmclass.register

  FM.add.class("lmfm", "lmRob")
  FM.add.class("lmfm", "lmrob")
  FM.add.class("lmfm", "rlm")

  FM.add.class("glmfm", "glmRob")
  FM.add.class("glmfm", "glmrob")

  FM.register("covfm", c("covRob",  "covClassic"))
  FM.register("fdfm",  c("fitdstnRob", "fitdstn"))
  ##--------------- end {fit.models} -------------------

  invisible()
}


### do everthing at *load* time
## .onAttach <- function(libname, pkgname)
## {
##   requireNamespace("fit.models")
##   FM.add.class      <- fit.models::fmclass.add.class
##   FM.register.class <- fit.models::fmclass.register.class

##   FM.add.class("lmfm", "lmRob")
##   FM.add.class("lmfm", "lmrob")
##   FM.add.class("lmfm", "rlm")

##   FM.add.class("glmfm", "glmRob")
##   FM.add.class("glmfm", "glmrob")

##   FM.register("covfm", c("covRob", "covClassic"))
##   FM.register("fdfm", c("fitdstnRob", "fitdstn"))

##   invisible()
## }



