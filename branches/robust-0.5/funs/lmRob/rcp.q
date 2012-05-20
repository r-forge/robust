#
#	Functions for Robust Model Selection
#	Contributed by Staudte and ???
#


RCp <- function(xfile, yfile, k = 1.345, b = 1.5, MEDIAN = T,
	iter = 20, nbest = 5, conv = 0.001,  tol = 1e-06, iter1 = 25,
	P2 = T, s = "subset", v1 = "huber", DIGITS = 2,
	rescale = T)
{
	if(s == "subset") {
		subset <- function(k)
		{
			m <- matrix(c(0, 1), nrow = 2)
			m1 <- for(i in 1:(k - 1)) {
				m <- rbind(cbind(0, m), cbind(1, m))
			}
			p <- rowSums(m) + 1
			s <- m[order(p),  ]
			s <- s[ - (p == 1),  ]
			mode(s) <- "logical"
			s
		}
		s <- subset(ncol(xfile))
	}


	Bfinal <- function(xfile, b = 1.5, tol = 1e-06, iter = 25, P = T, MEDIAN=T)
	{
		file <- NULL
		X <- cbind(1, xfile)
		tX <- t(X)
		B1 <- X %*% solve(tX %*% X)
		B1 <- chol(t(B1) %*% B1)
		it <- 0
		w <- rep(1, nrow(xfile))
		w1 <- NULL
		b1 <- (b * (ncol(xfile) + 1))/nrow(xfile)
		normdiff <- 1 + tol

		while(normdiff > tol && it <= iter) {
			it <- it + 1

			for(i in 1:nrow(xfile)) {
				yi <- as.matrix(B1 %*% tX[, i])
				w1[i] <- min(1, b1/sqrt(t(yi) %*% yi))
			}

			A1 <- tX %*% diag(w^2) %*% X
			A <- solve(A1)
			A <- .5 * (t(A) + A)
			B <- chol(A, pivot = P)

			if(P == T)
				B <- B[, order(attr(B, "pivot"))]

			Diff <- B - B1
			wdiff <- max(abs(w - w1))
			normdiff <- sum(diag(t(Diff) %*% Diff))
			w <- w1
			B1 <- B
			BX <- sqrt(diag(X %*% (t(B) %*% B) %*% t(X)))
			b1  <- ifelse(MEDIAN==T,b * median(BX), b*mean(BX))
		}

		file$BX <- BX
		file$B <- B
		file$w <- w
		file
	}

	assign("Bfinal", Bfinal, frame = 1)


	LABEL <- function(s, NAME = c(1, 2, 3, 4, 5, 6, 7, 8, 9,
		"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
		"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
		"W", "X", "Y", "Z"))
	{
		length(NAME) <- length(s)
		paste(NAME[s], collapse = " ")
	}


	HWT <- function(xfile, yfile, k = 1.345, iter = 20, conv = 0.01,
					w = rep(1, n), rescale = T)
	{
		k1 <- k
		mode(k1) <- "character"

		K <- switch(k1,
			"1" = c(.84932, .51606, .68269, .19875, .68269, .68269),
			"1.345" = c(.93258, .71016, .82137, .38703, .82137, .82137),
			"1.5" = c(.95431, .77846, .86639, .47783, .86639, .86639),
			"2" = c(.98846, .92054, .9545, .73854, .9545, .9545),
			"100" = c(1, 1, 1, 1, 1, 1))

		a1 <- K[1]
		b1 <- K[2]
		c1 <- K[3]
		d1 <- K[4]
		e1 <- K[5]
		f1 <- K[6]
		xfile <- as.matrix(xfile)
		n <- nrow(xfile)
		p <- ncol(xfile) + 1
		scale <- mad(lsfit(xfile, yfile)$residuals)
		hub <- NULL
		it <- 0

		repeat {
			ls <- lsfit(xfile, yfile, wt = w)
			r <- ls$residuals
			scale <- ifelse(rescale == T, mad(ls$residuals), scale)
			u <- r/scale
			w <- ifelse(abs(u) <= k, 1, k/abs(u))
			D <- max(abs(w - ls$wt))
			it <- it + 1
			if(D <= conv || it == iter + 1)
				break
		}

		hub$coef <- ls$coef
		hub$resid <- ls$residual
		hub$w <- ls$wt
		hub$D <- D
		hub$it <- it - 1
		hub$k <- k
		hub$VP <- (a1 * b1 * p)/(c1^2)
		hub$UP <- n * b1 - (2 * d1 * p)/c1 +
							(b1 * (e1 + 2 * f1 -3 * a1) * p) / (c1^2) +
							(a1 * b1 * p)/(c1^2)
		hub$WP <- sum((hub$resid * hub$w)^2)
		hub
	}


	MWT <- function(xfile, yfile, k = 1.345, iter = 20, b = 1.5, 
						MEDIAN = T, conv = 0.01, rescale = T, v1 = "h",
						tol = 1e-06, iter1 = 25, P2 = T)
	{

		#v1 can equal
		#"huber" when v1=1
		#"h" when v1=psi(h)/h
		#"sqrth" when v1=psi(sqrt(h))/sqrt(h)
		#"h/(1-h)" when v1=psi(h/(1-h))/(h/(1-h))
		#"Bx" when v1=psi(||Bx||)/||Bx|| (B-robust)

		k1 <- k
		mode(k1) <- "character"

		K <- switch(k1,
			"1" = c(.84932, .51606, .68269, .19875, .68269, .68269),
			"1.345" = c(.93258, .71016, .82137, .38703, .82137, .82137),
			"1.5" = c(.95431, .77846, .86639, .47783, .86639, .86639),
			"2" = c(.98846, .92054, .9545, .73854, .9545, .9545),
			"100" = c(1, 1, 1, 1, 1, 1))

		a1 <- K[1]
		b1 <- K[2]
		c1 <- K[3]
		d1 <- K[4]
		e1 <- K[5]
		f1 <- K[6]
		xfile <- as.matrix(xfile)
		n <- nrow(xfile)
		p <- ncol(xfile) + 1
		h <- hat(xfile)

		f <- switch(v1,
			huber = rep(1, n),
			h = h,
			sqrth = sqrt(h),
			"h/(1-h)" = h/(1 - h),
			Bx = Bfinal(xfile, b = b, tol = tol, iter = iter1,
				P = P2, MEDIAN = MEDIAN)$BX)

		b <- ifelse(MEDIAN == T, b * median(f), b * mean(f))
		v1 <- pmin(1, b/abs(f))
		w <- rep(1, n)
		it <- 0
		mal <- NULL
		scale <- mad(lsfit(xfile, yfile)$residuals)

		repeat {
			ls <- lsfit(xfile, yfile, wt = w)
			scale <- ifelse(rescale == T, mad(ls$residuals), scale)
			u <- ls$residuals/scale
			v2 <- ifelse(abs(u) <= k, 1, k/abs(u))
			w <- v1 * v2
			D <- max(abs(w - ls$wt))
			it <- it + 1
			if(D <= conv || it == iter + 1)
				break
		}

		mal$coef <- ls$coef
		mal$resid <- ls$residual
		mal$w <- ls$wt
		mal$v1 <- v1
		mal$v2 <- mal$w/v1
		mal$D <- D
		mal$it <- it - 1
		mal$k <- k1
		mal$b <- b
		X <- cbind(1, xfile)
		Z1 <- matrix(0, nrow = p, ncol = p)
		Z2 <- matrix(0, nrow = p, ncol = p)
		Z3 <- matrix(0, nrow = p, ncol = p)
		for(i in 1:n) {
			xi <- as.matrix(X[i,  ])
			vi <- v1[i]
			Z1i <- vi * xi %*% t(xi)
			Z1 <- Z1 + Z1i
			Z2i <- (vi^2) * xi %*% t(xi)
			Z2 <- Z2 + Z2i
			Z3i <- (vi^3) * xi %*% t(xi)
			Z3 <- Z3 + Z3i
		}
		Z1 <- Z1/n
		Z2 <- Z2/n
		Z3 <- Z3/n
		mal$G <- sum(diag((Z2 %*% solve(Z1)) %*% (Z2 %*% solve(Z1))))
		mal$H <- sum(diag((Z3 %*% solve(Z1))))
		mal$L <- sum(v1^2)
		mal$VP <- (a1 * b1 * mal$G)/(c1^2)
		mal$UP <- b1 * mal$L - (2 * d1 * mal$H)/c1 + (b1 * 
		(e1 + 2 * f1 - 3 *a1) * mal$G)/(c1^2) + (a1 * b1 * mal$G)/(c1^2)
		mal$WP <- sum((mal$w * mal$resid)^2)
		mal$method <- "mallows"
		mal$k <- k
		mal
	}

	## Beginning of RCp ##

	#v1 can equal
	#"huber" when v1=1
	#"h" when v1=psi(h)/h
	#"sqrth" when v1=psi(sqrt(h))/sqrt(h)
	#"h/(1-h)" when v1=psi(h/(1-h))/(h/(1-h))
	#"Bx" when v1=psi(||Bx||)/||Bx|| (B-robust)

	weights <- ifelse(v1 == "huber", "huber", "mallows")
	file <- NULL

	FULL <- switch(weights,
		huber = HWT(xfile, yfile, k = k, conv = conv, iter = iter,
			rescale = rescale),
		mallows = MWT(xfile, yfile, k = k, iter = iter, b = b,
			conv = conv, rescale = T, v1 = v1,  tol = tol,
			iter1 = iter1, P2 = P2))

	sighat <- sqrt(FULL$WP/FULL$UP)
	file$sighat <- sighat
	P <- rowSums(s) + 1
	N <- nrow(xfile)
	K <- ncol(xfile) + 1

	for(i in 2:(K - 1)) {
		S <- s[P == i,  ]
		if(length(S) > 0) {
			D <- min(nrow(S), (nbest + 1))
			LB <- NULL
			RC <- NULL
			VP <- NULL
			UP <- NULL
			res <- matrix(0, nrow = D, ncol = N)
			C <- matrix(0, nrow = D, ncol = K)
			which <- matrix(0, nrow = D, ncol = (K - 1))
			W <- matrix(0, nrow = D, ncol = N)
			V1 <- matrix(0, nrow = D, ncol = N)
			V2 <- matrix(0, nrow = D, ncol = N)

			for(l in 1:nrow(S)) {
				x <- as.matrix(xfile[, S[l,  ]])

				R <- switch(weights,
					huber = HWT(x, yfile, k = k, conv = conv,
						iter = iter, rescale = rescale),
					mallows = MWT(x, yfile, k = k, b = b,
						conv = conv, iter = iter, MEDIAN = MEDIAN,
						rescale = rescale, v1 = v1))

				j <- min(l, D)
				LB[j] <- LABEL(S[l,  ])
				RC[j] <- R$WP/(sighat^2) - (R$UP - R$VP)
				VP[j] <- R$VP
				UP[j] <- R$UP
				W[j,  ] <- R$w

				V1[j,  ] <- switch(weights,
					huber = rep(1, nrow(xfile)),
					mallows = R$v1)

				V2[j,  ] <- switch(weights,
					huber = W[j,  ],
					mallows = R$v2)

				C[j, c(T, S[l,  ])] <- R$coef
				res[j,  ] <- R$resid
				which[j,  ] <- S[l,  ]
				if(j == D) {
					ord <- order(RC)
					RC <- RC[ord]
					VP <- VP[ord]
					UP <- UP[ord]
					LB <- LB[ord]
					V1 <- V1[ord,  ]
					V2 <- V2[ord,  ]
					W <- W[ord,  ]
					which <- which[ord,  ]
					C <- C[ord,  ]

					if(D < nrow(S)) {
						C[D,  ] <- rep(0, K)
						which[D,  ] <- rep(0, (K - 1))
					}

					res <- res[ord,  ]
				}
			}

			l <- min(nbest, nrow(S))
			ord <- order(RC)[1:l]
			size <- rep(i, l)
			file$size <- c(file$size, size)
			file$Vp <- c(file$Vp, VP[ord])
			file$Up <- c(file$Up, UP[ord])
			file$RCp <- c(file$RCp, RC[ord])
			file$label <- c(file$label, LB[ord])
			file$coef <- rbind(file$coef, C[ord,  ])
			file$res <- rbind(file$res, res[ord,  ])
			file$w <- rbind(file$w, W[ord,  ])
			file$v1 <- rbind(file$v1, V1[ord,  ])
			file$v2 <- rbind(file$v2, V2[ord,  ])
			file$which <- rbind(file$which, which[ord,  ])
		}
	}
	file$size <- c(file$size, K)
	RC <- FULL$WP/(sighat^2) - (FULL$UP - FULL$VP)
	file$RCp <- c(file$RCp, RC)
	LB <- LABEL(rep(T, ncol(xfile)))
	file$label <- c(file$label, LB)
	file$Vp <- c(file$Vp, FULL$VP)
	file$Up <- c(file$Up, FULL$UP)
	file$res <- round(rbind(file$res, t(as.matrix(FULL$resid))),
		digits =DIGITS)
	file$which <- rbind(file$which, rep(T, (K - 1)))
	file$coef <- round(rbind(file$coef, t(as.matrix(FULL$coef))),
		digits = DIGITS)
	file$w <- round(rbind(file$w, t(as.matrix(FULL$w))),
		digits = DIGITS)

	V1 <- t(as.matrix(switch(weights,
		huber = rep(1, nrow(xfile)),
		mallows = FULL$v1)))

	V2 <- t(as.matrix(switch(weights,
		huber = FULL$w,
		mallows = FULL$v2)))

	file$v1 <- round(rbind(file$v1, V1), digits = DIGITS)
	file$v2 <- round(rbind(file$v2, V2), digits = DIGITS)
	file$method <- v1
	oldClass(file) <- "RCp"
	file
}


plot.RCp <- function(Rfile, nbest = 5, J = N + 1, L = N + 1, a = 3.5, CEX = 1)
{
	par(cex = CEX)
	Rfile <- summary.RCp(Rfile, best = nbest)
	N <- max(Rfile$size)
	R <- NULL
	M1 <- min(Rfile$RCp)
	M <- min(0, M1)
	R$RCp <- Rfile$RCp[Rfile$RCp <= J]
	R$Vp <- Rfile$Vp[Rfile$RCp <= J]
	R$label <- Rfile$label[Rfile$RCp <= J]
	plot(R$Vp, R$RCp, type = "n", xlab = "", ylab = "",
		ylim = c(M, J), xlim = c(0, L))
	mtext("RCp", side = 2, line = a)
	mtext("Vp", side = 1, line = a)
	text(R$Vp, R$RCp, R$label)
	title(main = "RCp vs Vp")
	abline(0, 1)
	invisible(Rfile)
}


summary.RCp <- function(file, best = 5)
{
	select.best <- function(file, best) {
		B <- NULL
		K <- max(file$size)
		l <- 1:length(file$size)

		for(i in 2:K) {
			Bi <- l[file$size == i]
			b <- min(best, length(Bi))

			if(b > 0) 
				Bi <- Bi[1:b]

			B <- c(B, Bi)
		}
		B
	}

	B <- select.best(file, best = best)
	file$RCp <- file$RCp[B]
	file$size <- file$size[B]
	file$label <- file$label[B]
	file$Vp <- file$Vp[B]
	file$Up <- file$Up[B]
	file$coef <- file$coef[B, , drop = F]
	file$w <- file$w[B, , drop = F ]
	file$v1 <- file$v1[B, , drop = F]
	file$v2 <- file$v2[B, , drop = F]
	file$res <- file$res[B, , drop = F ]
	file$which <- file$which[B, , drop = F]
	oldClass(file) <- "summary.RCp"
	file
}


#plot.CP <- function(xfile, yfile, nbest = 5, WT = rep(1, length(yfile)),
#	J = N +1, L = N + 1, a = 3.5, CEX = 1)
#{
#	par(cex = CEX)
#	K <- ncol(xfile) + 1
#	N <- ncol(xfile)
#	C <- leaps(xfile, yfile, nbest = nbest, wt = WT)
#	Cp1 <- NULL
#	M1 <- min(C$Cp)
#	M <- min(0, M1)
#	Cp1$size <- C$size[C$Cp <= J]
#	Cp1$label <- C$label[C$Cp <= J]
#	Cp1$Cp <- C$Cp[C$Cp <= J]
#	plot(Cp1$size, Cp1$Cp, type = "n", xlab = "", ylab = "",
#		ylim = c(M, J), xlim = c(0, L))
#	mtext("Cp", side = 2, line = a)
#	mtext("p", side = 1, line = a)
#	text(Cp1$size, Cp1$Cp, Cp1$label)
#	abline(0, 1)
#}


print.summary.RCp <- function(x, ...)
{
	for(name in names(x)) {
		cat(paste("\n", name, ":\n", sep = ""))
		print(x[[name]])
	}

	invisible(x)
}


print.RCp <- function(x, ...)
{
	for(name in names(x)) {
		cat(paste("\n", name, ":\n", sep = ""))
		print(x[[name]])
	}

	invisible(x)
}
