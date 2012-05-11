get.fit.models.database <- function()
{

  fmdb <- list()

  ##
  ##  This function returns a list containing one element for
  ##  each set of comparable models.  These elements are in
  ##  turn lists with three components: (1) a character vector
  ##  containing the classes in each set, (2) a character vector
  ##  with one element containing the virtual class whose methods
  ##  should be used, and (3) a validation function.
  ##

  ##  Add linear models

  fmdb[["lmfm"]] <-
            list(classes = c("lmRob", "lmrob", "lm"),
                 object.class = "lmfm",
                 validation.function = NULL,
                 attributes.function = NULL)

  ##  Add generalized linear models

  fmdb[["glmfm"]] <-
            list(classes = c("glmRob", "glm"),
                 object.class = "glmfm",
                 validation.function = NULL,
                 attributes.function = NULL)

  ##  Add AOV models

  fmdb[["aovfm"]] <-
            list(classes = c("aovRob", "aov"),
                 object.class = "aovfm",
                 validation.function = NULL,
                 attributes.function = NULL)

  ##  Add AOV models

  fmdb[["discfm"]] <-
            list(classes = c("discRob", "discrim"),
                 object.class = "discfm",
                 validation.function = NULL,
                 attributes.function = NULL)

  ##  Add covariance models

  fmdb[["covfm"]] <-
            list(classes = c("covRob", "cov", "ccov"),# renamed cov() to ccov()
                 object.class = "covfm",
                 validation.function = NULL,
                 attributes.function = NULL)

  ##  Add fitdistr-like models

  classes <- c("fitdistrRob", "fitdistr")
  object.class <- "fdfm"

  attributes.function <- function(model.list, fm.call, attributes) {
    if(is.null(fm.call[["densfun"]]))
      distribution <- as.character(fm.call[[4]])
    else
      distribution <- as.character(fm.call[["densfun"]])

    if(is.null(fm.call[["x"]]))
      data.name <- as.character(fm.call[[3]])
    else
      data.name <- as.character(fm.call[["x"]])

    x <- get(data.name)

    attrs <- attributes(model.list)
    attrs[["distribution"]] <- distribution
    attrs[["data.name"]] <- data.name
    attrs[["x"]] <- x
    attributes(model.list) <- attrs

    model.list
  }

  fmdb[["fdfm"]] <- list(classes = classes,
                         object.class = "fdfm",
                         validation.function = NULL,
                         attributes.function = attributes.function)

  ##  Add principal components models

  classes <- c("princompRob", "princomp")
  object.class <- "pcompfm"

  fmdb[["pcompfm"]] <- list(classes = classes,
                          object.class = object.class,
                          validation.function = NULL,
                          attributes.function = NULL)

  fmdb
}


