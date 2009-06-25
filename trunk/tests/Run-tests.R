library(robust)

sessionInfo()

tDir <- system.file("tests_S", package = "robust")

tstFiles <- list.files(tDir, pattern = "\\.t$")
## Remove those that are not (yet) available for package 'robust' :
(tstFiles <- tstFiles[! match(tstFiles, c("asymmetric.t"), nomatch=0)])

for(f in tstFiles) {
    cat("Test File", f, ":\n")

    exps <- parse(file = file.path(tDir, f), srcfile=NULL)
    for(i in seq_along(exps)) {
        cat(" ")
        if(!isTRUE(eval(exps[[i]]))) {
            ch.ex <- paste(substr(paste(format(exps[[i]])[-1], collapse = " ; "), 1, 60), "...")
            stop("*** ", ch.ex,"  was *not* TRUE")
        }
        cat(".")
    }
    cat("\n")
}
