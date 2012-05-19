fit.models <- function(model.list, ..., attributes = NULL)
{
  fm.call <- match.call()
  fm.call$attributes <- NULL
  dots <- list(...)

  ## get database of camparable models - eventually this should reside in
  ## its own environment and be user-modifiable
  fmdb <- get.fit.models.database()

  comparables <- lapply(fmdb, function(u) u$classes)
  supported.classes <- unique(unlist(comparables))
  vclass <- NULL


  ## If model.list is missing, assume that dots contains a named "list" of
  ## fitted models.

  if(missing(model.list))
    model.list <- dots


  ## If model.list is a fitted model, assume any remaining arguments are also
  ## fitted models: try to combine them into a fit.models object.

  else if(class(model.list)[1] %in% supported.classes) {
    model.list <- fit.models(".temp-name" = model.list, ...)
    mod.names <- c("", names(fm.call)[-(1:2)])
    name.lengths <- nchar(mod.names)
    no.names <- which(name.lengths == 0)
    model.funs <- as.character(fm.call)[-1]
    mod.names[no.names] <- model.funs[no.names]
    names(model.list) <- mod.names
  }


  # If model.list contains a list of comparable functions

  else {
    n.models <- length(model.list)

    if(is.null(names(model.list)))
      names(model.list) <- sapply(model.list, function(u) as.character(u[[1]]))

    mod.names <- names(model.list)
    name.lengths <- nchar(mod.names)
    no.names <- which(name.lengths == 0)
    model.funs <- as.character(model.list)

    if(length(no.names)) {
      mod.names[no.names] <- model.funs[no.names]
      names(model.list) <- mod.names
    }

    ## Quick fix for R ##
    model.list[model.list == "cov"] <- "covMLE"

    #extract model functions and check comparability
    idx <- sapply(comparables, function(u, w) all(w %in% u), w = model.funs)

    if(!any(idx))
      stop("models are not comparable")

    # we get the virtual class here for free
    vclass <- (names(comparables)[idx])[1]

    # replace each element of model.list with a function call
    model.args <- as.list(fm.call)[-(1:2)]
    for(i in 1:n.models)
      model.list[[i]] <- do.call(model.list[[i]], model.args)
  }

  if(is.null(vclass)) {
    model.funs <- sapply(model.list, function(u) as.character(u$call[[1]]))
    idx <- sapply(comparables, function(u, w) all(w %in% u), w = model.funs)

    if(!any(idx))
      stop("models are not comparable")

    vclass <- (names(comparables)[idx])[1]
  }

  if(!is.null(is.valid <- fmdb[[vclass]]$validation.function))
    is.valid(model.list)

  if(!is.null(add.attributes <- fmdb[[vclass]]$attributes.function))
    model.list <- add.attributes(model.list, fm.call, attributes)

  oldClass(model.list) <- vclass
  model.list
}


