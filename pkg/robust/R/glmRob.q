glmRob <- function(formula, family = binomial(), data, weights, subset,
                   na.action, method = "cubif", model = TRUE, x = FALSE,
                   y = TRUE, control = glmRob.control, contrasts = NULL,
                   ...)
{
  the.call <- match.call()

  if(is.character(family))
    family <- get(family, mode = "function", envir = parent.frame())
  if(is.function(family))
    family <- family()
  if(is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  if(is.function(control))
    control <- control(method, ...)

  m <- call("model.frame", formula = substitute(formula))

  if(!missing(data))
    m$data <- substitute(data)

  if(!missing(na.action))
    m$na.action <- substitute(na.action)

  if(!missing(subset))
    m$subset <- substitute(subset)

  m <- eval(m, sys.parent())
  Terms <- attr(m, "terms")
  a <- attributes(m)
  Y <- model.extract(m, "response")
  X <- model.matrix(Terms, m, contrasts)
  offset <- model.extract(m, offset)

  fit <- switch(method,

    "cubif" = glmRob.cubif(x = X,
                           y = Y,
                           intercept = FALSE,
                           offset = offset,
                           family = family,
                           null.dev = TRUE,
                           control = control),

    "misclass" = glmRob.misclass(x = X,
                                 y = Y,
                                 control = control,
                                 offset = offset,
                                 null.dev = TRUE,
                                 family = family,
                                 Terms = Terms),

    "mallows" = glmRob.mallows(x = X,
                               y = Y,
                               control = control,
                               offset = offset,
                               null.dev = TRUE,
                               family = family,
                               Terms = Terms),

    ## otherwise
    stop("invalid 'method'")
  )

  fit$terms <- Terms
  fit$formula <- substitute(formula)
  fit$method <- method
  fit$call <- the.call
  fit$control <- control

  if(model)
    fit$model <- m
  if(x)
    fit$x <- X
  if(!y)
    fit$y <- NULL

  oldClass(fit) <- "glmRob"

  fit
}
