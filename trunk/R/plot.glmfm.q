plot.glmfm <- function(x, which.plots = "ask",  ...)
{
  n.models <- length(x)

  choices <- c("All",
               "Deviance Residuals vs. Fitted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Pearson Residuals",
               "QQ Plot of Deviance Residuals",
               "Deviance Residuals vs. Robust Distances",
               "Deviance Residuals vs. Index (Time)",
               "Square Root of Absolute Deviance Residuals vs. Fitted Values")

  all.plots <- 2:length(choices)

  tmenu <- paste("plot:", choices)

  if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, all.plots)

    if(length(which.plots) == 0)
      return(invisible(x))

    if(length(which.plots) > 1) {
      par.ask <- par(ask = TRUE)
      on.exit(par(ask = par.ask))
    }

    ask <- FALSE
    which.plots <- c(which.plots + 1, 1)
  }

  else if(which.plots == "all") {
    which.plots <- c(all.plots + 1, 1)
    ask <- FALSE
    par.ask <- par(ask = TRUE)
    on.exit(par(ask = par.ask))
  }

  else
    ask <- TRUE

  repeat {
    if(ask) {
      which.plots <- menu(tmenu,
                          title = "\nMake plot selections (or 0 to exit):\n")

      if(any(which.plots == 1)) {
        which.plots <- c(all.plots, 0)
        par.ask <- par(ask = TRUE)
        on.exit(par(ask = par.ask))
      }

      which.plots <- which.plots + 1
    }

    for(pick in which.plots) {
      switch(pick,
        return(invisible(x)),

        place.holder <- 1,

        lmfmResVsFittedPlot(x,
                            type = "deviance",
                            xlab = "Fitted Values",
                            ylab = "Deviance Residuals",
                            main = "Deviance Residuals vs. Fitted Values",
                            pch = 16,
                            ...),

        lmfmRespVsFittedPlot(x, 
                             xlab = "Fitted Values",
                             ylab = "Response",
                             main = "Response vs. Fitted Values",
                             pch = 16,
                             ...),

        lmfmResQQPlot(x,
                      type = "pearson",
                      xlab = "Standard Normal Quantiles",
                      ylab = "Ordered Pearson Residuals",
                      main = "Normal QQ Plot of Pearson Residuals",
                      envelope = FALSE,
                      pch = 16,
                      ...),

        {
          fam <- family(x[[1]])$family
          r <- as.matrix(sapply(x, residuals, type = "deviance"))
          f <- as.matrix(sapply(x, fitted))
          n <- nrow(f)
          mod.names <- names(x)
          dist <- ifelse(fam == "Binomial", 0, 1)
          qq.a <- y <- matrix(0, n, n.models)

          for(i in 1:n.models) {
            y[,i] <- x[[i]]$y
            qq.a[order(r[, i]), i] <- qqplot.glmRob(y[,i], f[,i], dist)$quantiles
          }

          panel.special <- function(x, y, id.n = 3) {
            panel.xyplot(x, y, pch = 16)
            panel.addons(x, y, smooths = FALSE, rugplot = FALSE, id.n = id.n)
            invisible()
          }

          mod <- factor(rep(mod.names, each = n), levels = mod.names)

          df <- data.frame(qq.a = as.vector(qq.a),
                           qq.b = as.vector(r),
                           mod = mod)

          p <- xyplot(qq.a ~ qq.b | mod,
                      data = df,
                      xlab = "Theoretical Quantiles",
                      ylab = "Ordered Deviance Residuals",
                      main = "QQ Plot of Deviance Residuals",
                      panel = panel.special,
                      strip = function(...) strip.default(..., style = 1),
                      layout = c(n.models, 1, 1),
                      ...)

          print(p)
        },

        lmfmResVsRDPlot(x,
                        type = "deviance",
                        xlab = "Robust Distances",
                        ylab = "Deviance Residuals",
                        main = "Deviance Residuals vs. Robust Distances",
                        pch = 16,
                        ...),

        lmfmResVsIdxPlot(x,
                         type = "deviance",
                         xlab = "Index (Time)",
                         ylab = "Deviance Residuals",
                         main = "Deviance Residuals vs. Index (Time)",
                         pch = 16,
                         ...),

        lmfmSqrtResVsFittedPlot(x,
                                type = "deviance",
                                xlab = "Fitted Values",
                                ylab = expression(sqrt(abs(plain("Deviance Residuals")))),
                                main = "Square Root of Deviance Residuals vs. Fitted Values",
                                pch = 16,
                                ...)
      ) ## switch(pick, ..)
    } ## end for(...)
  } ## repeat {...}
  invisible(x)
}


