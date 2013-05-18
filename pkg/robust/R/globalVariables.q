## The codetools package sometimes falsely identifies variables as global.
## This file is intended to hide these errors from R CMD check.  It should
## be deleted while testing the package but restored before submitting to
## CRAN.

utils::globalVariables(c("mod"))


