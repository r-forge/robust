print.aovfm <- function(x, intercept = FALSE, ...)
{
	n.models <- length(x)
	digits <- options()$digits - 3
	model.list <- attr(x, "model.list")
	mod.names <- format(names(model.list))

	cat("\nCalls: \n")
	for(i in 1:n.models) {
		cat(mod.names[i], ": ")
		print(x[[i]]$call)
	}

	effects <- lapply(x, function(x) x$effects)
	asgn <- lapply(x, function(x) x$assign)
	rl <- unique(unlist(lapply(asgn, names)))
	nterms <- length(rl)
	df <- ss <- matrix(NA, ncol = nterms, nrow = n.models)
		if(nterms) {
			for(j in 1:n.models) {
			  tmp <- match(names(asgn[[j]]), rl)
			  for(i in 1:length(tmp)) {
			    ai <- asgn[[j]][[i]]
			    df[j, tmp[i]] <- length(ai)
			    ss[j, tmp[i]] <- sum(effects[[j]][ai]^2)
			  }
			}
			keep <- df > 0
			for(j in 1:n.models) {
			  if(!intercept && names(asgn[[j]])[1] == "(Intercept)")
			    keep[j, 1] <- F
			}
			df[as.vector(!keep)] <- NA
			ss[as.vector(!keep)] <- NA
			keep <- apply(keep, 2, any)
			rl <- rl[keep]
			nterms <- length(rl)
			df <- df[, keep]	
			ss <- ss[, keep]
		}

		cat("\nTerms:\n")

		if(nterms == 0) { #empty model
			prmatrix(array(0, c(1, 2), list("<empty>", c("Sum of Squares", 
			  "Deg. of Freedom"))))
			return(invisible(x))
		}

		df.res <- sapply(x, function(x) x$df.resid)

		if(any(df.res > 0)) {
			nterms <- nterms + 1
			rl[nterms] <- "Residuals"
			df <- cbind(df, df.res)
			ss <- cbind(ss, sapply(x, function(x)
			sum((x$resid)^2)))
		}

		ss <- zapsmall(ss)
		tmp <- rbind(format(ss), format(df))
		dimnames(tmp) <- list(c(paste(mod.names, "Sum of Squares"),
			paste(mod.names, "Deg. ofFreedom")), rl)
		prmatrix(tmp, quote = F, right = T)
		cat("\n")

# residual standard errors

	sigmas <- rep(NA, n.models)
	for(i in 1:n.models) {
		if(model.list[[i]][[1]] == "lm" || model.list[[i]][[1]] == "aov")
		  sigmas[i] <- sqrt(sum(x[[i]]$residuals^2)/x[[i]]$df.resid)
		else sigmas[i] <- x[[i]]$scale
	}

	if(any(!is.na(sigmas))) {
		cat("Residual standard errors:\n")
		for(i in 1:n.models)
		  cat(mod.names[i], ":", format(signif(sigmas[i], digits)), "on", 
		    x[[i]]$df.resid, "degrees of freedom\n")
	}

	invisible(x)
}


