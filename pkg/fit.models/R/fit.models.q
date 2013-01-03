fit.models <- function(model.list, ...)   #, attributes = NULL)
{
  fm.call <- match.call()
  fm.call$attributes <- NULL

  dots <- list(...)
  dots.names <- names(dots)

#  if(is.null(attributes))
#    attributes <- list()

  if(is.null(dots.names))
    dots.names <- character(length(dots))

  fmreg <- get("fmreg", pos = fit.models:::fm.registry)
  supported.classes <- unlist(sapply(fmreg, function(u) u$classes))


  ## The only way model list can be missing is if all the arguments in the call
  ## are named. In this case, assume dots is a collection of comparable fitted
  ## models.

  if(missing(model.list)) {
    model.list <- dots
    model.names <- dots.names
  }


  ## Otherwise model.list is either an unnamed (in the call) fitted model or
  ## a list of function names.

  else if(class(model.list)[1] %in% supported.classes) {
    model.list <- c(list(model.list), dots)
    model.names <- c("", dots.names)

    object.names <- as.character(fm.call[-1])
    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- object.names[empty.names]
  }


  else if(is.character(model.list) || class(model.list)[1] == "list") {
    model.list <- as.list(model.list)
    n.models <- length(model.list)
    model.funs <- unlist(model.list)

    if(is.null(model.names <- names(model.list)))
      model.names <- model.funs

    model.call <- fm.call
    model.call$model.list <- NULL

    for(i in 1:n.models) {
      model.call[[1]] <- as.name(model.list[[i]])
      model.list[[i]] <- eval(model.call, sys.parent())
    }

    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- model.funs[empty.names]
  }

  else
    cat("Assert: bigo-error, this should never happen!\n\n")

  if(any(nchar(model.names) == 0))
    stop("All models should be named")


  ## Each element of model.list should have a unique name.

  if(length(unames <- unique(model.names)) < length(model.list)) {
    for(n in unames) {
      idx <- (model.names == n)
      if(sum(idx) > 1)
        model.names[idx] <- paste(n, 1:sum(idx), sep = ".")
    }
  }

  names(model.list) <- model.names


  ## Now we should have a properly named list of fitted models.  Have to
  ## set the appropriate attributes.

#  if(is.null(attributes$fmclass)) {
    candidates <- lapply(fmreg, getElement, name = "classes")
    classes <- sapply(model.list, function(u) class(u)[1])


    ## First, the fm class must beable to compare all the classes.

    idx <- sapply(candidates, function(u) all(classes %in% u))
    candidates <- candidates[idx]

    if(!length(candidates)) {
      warning("fit.models cannot compare the provided models")
      return(invisible(model.list))
    }


    ## Try to choose the best comparable class.

    idx <- sapply(candidates, function(u) length(intersect(u, classes)))
    #attributes$
    fmclass <- names(which(idx == max(idx)))[1]
#  }

  #base::attributes(model.list) <- attributes
  oldClass(model.list) <- fmclass
  model.list
}

