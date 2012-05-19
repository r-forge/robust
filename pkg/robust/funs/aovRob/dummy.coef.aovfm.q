dummy.coef.aovfm <- function(object)
{
  n.models <- length(object)
  mod.names <- names(object)
  table.list <- list()

  for(i in 1:n.models) {
    oldClass(object[[i]]) <- "aov"
    table.list[[i]] <- dummy.coef(object[[i]])
  }

  n.coef <- length(table.list[[1]])
  if(n.models > 1) {
    for(i in 1:n.coef) {
      for(j in 2:n.models) {
        table.list[[1]][[i]] <- rbind(table.list[[1]][[i]], table.list[[j]][[i]])
      }
      dimnames(table.list[[1]][[i]]) <- list(mod.names, dimnames(table.list[[1]][[i]])[[2]])
    }
  }
  table.list[[1]]
}


