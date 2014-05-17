anova.glmRoblist <- function(object, ..., test = c("none", "Chisq", "F", "Cp"))
{
  diff.term <- function(term.labels, i)
  {
    t1 <- term.labels[[1]]
    t2 <- term.labels[[2]]
    m1 <- match(t1, t2, FALSE)
    m2 <- match(t2, t1, FALSE)
    if(all(m1)) {
      if(all(m2))
        return("=")
      else return(paste(c("", t2[ - m1]), collapse = "+"))
    }
    else {
      if(all(m2))
        return(paste(c("", t1[ - m2]), collapse = "-"))
      else return(paste(i - 1, i, sep = " vs. "))
    }
  }

  test <- match.arg(test)
  rt <- length(object)

  if(rt == 1) {
    object <- object[[1]]
    anova(object, ...)
  }

  forms <- sapply(object, function(x) as.character(formula(x)))
  subs <- as.logical(match(forms[2,  ], forms[2, 1], FALSE))
  if(!all(subs))
    warning("Some fit objects deleted because response differs from the first model")
  if(sum(subs) == 1)
    stop("The first model has a different response from the rest")
  forms <- forms[, subs]
  object <- object[subs]
  dfres <- sapply(object, "[[", "df.residual")
  dev <- sapply(object, "[[", "deviance")
  tl <- lapply(object, labels)
  rt <- length(dev)
  effects <- character(rt)
  for(i in 2:rt)
    effects[i] <- diff.term(tl[c(i - 1, i)], i)
  ddev <- -diff(dev)
  ddf <- -diff(dfres)
  heading <- c("Analysis of Deviance Table", paste("\nResponse: ", forms[2, 1], "\n", sep = ""
    ))
  aod <- data.frame(Terms = forms[3,  ], "Resid. Df" = dfres, "Resid. Dev" = dev, Test = 
    effects, Df = c(NA, ddf), Deviance = c(NA, ddev), check.names = FALSE)
# aod <- as.anova(aod, heading)
  if(test != "none") {
    n <- length(object[[1]]$residuals)
    o <- order(dfres)
    stat.anova(aod, test, deviance(object[[o[1]]])/dfres[o[1]], dfres[o[1]], n)
  }
  else aod
}


