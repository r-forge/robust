# TMLn.s

S.hysest <- function(X, y, iopt=1, nrep, nq=ncol(X)+1, ips=2, xk=1.5477, beta=0.5,
							time=F, intch=1, tolo=0.0001, tols=0.0001, tau=1.5e-6, seed=1313,
							gam=1, maxs=30, maxt=30, maxr=1)
{
       np  <- ncol(X); n <- nrow(X)
       mdi <- np + nq; ncov <- np*(np+1)/2
       mdw <- (np + 2) * nq + (n+3) * np + n
       z <- .Fortran("s_hsesm3",
						x=as.double(X),
						y=as.double(y),
            n=as.integer(n),
						np=as.integer(np),
						nq=as.integer(nq),
						ncov=as.integer(ncov),
            mdx=as.integer(n),
						mdw=as.integer(mdw),
						mdi=as.integer(mdi),
						iopt=as.integer(iopt),
            intch=as.integer(intch),
						nrep=as.integer(nrep),
						tols=as.double(tolo),
            tolr=as.double(tols),
						tau=as.double(tau),
						gam=as.integer(gam),
						maxit=as.integer(maxt),
            maxs1=as.integer(maxs),
						maxs2=as.integer(maxr),
						iseed=as.integer(seed),
						ierr=integer(1),
            smin=double(1),
						theta=double(n),
						rs=double(n),
						it1=integer(nq),
						cov=double(ncov),
            work=double(mdw),
						iwork=integer(mdi),
						ips=as.integer(ips),
						xk=as.double(xk),
            beta=as.double(beta),
						bet0=as.double(beta),
						itrace=as.integer(time))

	list(theta=z$theta[1:np], smin=z$smin, rs=z$rs, indices=z$it1, cov=z$cov, ierr=z$ierr)
}

# Main function
#--------------

TML.gauss <- function(X,y,cu=2.5,initial=c("S","input"),otp=c("adaptive","fixed"),
             cov=c("no","parametric","nonparametric"),input=NULL,iv=1,nrep=0,seed=1313){
#
# Main function for TML-estimates; Gaussian case
#                
n   <- length(y); np <- ncol(X); cl <- -cu
ips <- 2; xk <- 1.5477; beta <- 0.5 
# Step 1: Initial high bdp estimate
if (initial=="S")     {if (np <= 2 & n <= 500) iopt <- 3 else iopt <- 1; if (nrep!=0) iopt <- 2
                       z   <- S.hysest(X,y,iopt,nrep,np+1,ips,xk,beta,time=F,seed=seed)
                       th0 <- z$theta; v0  <- z$smin}
if (initial=="input") {z   <- input
                       th0 <- z$lambda;      v0 <- z$sigma}
namat <- matrix(NA,nrow=np,ncol=np)
nares <- list(th0=th0,v0=v0,th1=rep(NA,np),v1=NA,tl=NA,tu=NA,CV0=namat,V0=NA,CV1=namat,V1=NA,
         alpha=NA,tn=NA,beta=NA,wi=rep(NA,n))
# Step 2: rejection rule
re    <- as.vector(y-X%*%as.matrix(th0)); rs <- re/v0
tp    <- adaptn(sort(rs),cl,cu,option=otp); if (is.na(tp$tu)) return(nares)
wi    <- tPsin(rs,tp$tl,tp$tu)
yr    <- y[wi!=0 | rs==0]
Xr    <- X[wi!=0 | rs==0,,drop=F]
tp$tn <- length(yr)
# Step 3: ML-estimate on retained observations
z     <- MLnp(Xr,yr,iv,tp)
res   <- list(th0=th0,v0=v0,th1=z$th1,v1=z$v1,tl=tp$tl,tu=tp$tu,
         alpha=tp$alpha,tn=tp$tn,beta=tp$beta,wi=(wi!=0)*1)
if (cov!="no") {l <- cl; u <- cu; if (otp=="adaptive") {l <- tp$tl; u <- tp$tu} 
  if (cov=="parametric"   ) K <-Cov2.n(X,y,u,z$th1,z$v1,opt="integrals")
  if (cov=="nonparametric") K <-Cov2.n(X,y,u,z$th1,z$v1,opt="averages")
res  <- c(res,list(CV0=K$CV0,CV1=K$CV1))}
res} 

# Auxiliary functions for TML-estimation
#------------------------------------------------------------------------------

MLnp <- function(Xr,yr,iv,tp){np <- ncol(Xr)
# Maximum likelihood
beta <- tp$beta; tn <- tp$tn
# coefficients
# z  <- riclls(Xr,yr); th <- z$theta[1:np] 
  z  <- lsfit(Xr, yr, intercept=F)
  th <- z$coef
# scale
  if (iv==1) {rr  <- as.vector(yr-Xr%*%as.matrix(th)); v <- Scalen(rr,tn-np,beta)}
list(th1=th,v1=v)}

tPsin <- function(r,tl,tu){
# Weight function
nr   <- length(r)
tmp  <- rep(0,nr)
ind  <- (1:nr)[r>tl & r<tu]
tmp[ind] <- r[ind]; tmp}

Discr <- function(yo,Fo,cu){
# minimum ratio between Fn and Fo (for argument > cu)
n  <- length(yo)
# if (cu > yo[n]) {cat("cu is too large ","\n"); return(list(j=NA))}
Fn <- (0:(n-1))/n; r  <- Fn/Fo
j  <- (1:n)[yo > cu]
if (length(j)>0) {j <- j[1]; rj <- r[j:n]} else {j <- n+1; rj <- 1}
r.min <- min(rj,1)
if (r.min < 1) j.min <- (j:n)[rj==r.min] else j.min <- NULL
list(j=j,j.min=j.min,r.min=r.min)}

adaptn <- function(rso,cl,cu,option){
# adaptive cut-off
alpha  <- NA; n <- length(rso)
if (option=="adaptive"){
 rhos  <- sort(rso^2/2)
 Fo    <- pchisq(2*rhos,df=1) 
 dsr   <- Discr(rhos,Fo,cu^2/2); if (is.na(dsr$j)) return(list(tu=NA))
 alpha <- dsr$r.min
 tu2   <- as.numeric(quantile(rhos,probs=alpha)); tu <- sqrt(2*tu2)
 tu    <- max(tu,cu); tl    <- -tu}
if (option=="fixed"){tu <- cu; tl <- cl}
Beta  <- -tu*dnorm(tu)+tl*dnorm(tl)+pnorm(tu)-pnorm(tl)
beta  <-  Beta/(pnorm(tu)-pnorm(tl))
list(cl=cl,cu=cu,tl=tl,tu=tu,alpha=alpha,beta=beta)}

Scalen <- function(r,n,beta){sqrt(sum(r^2)/(n*beta))}

# Auxiliary functions for covariance matrix of estimates
#------------------------------------------------------------------------------

Pspphi.n  <- function(z) {psp.weight(z,ips=2,xk=1.5477)*dnorm(z)}
Psizphi.n <- function(z) {psi.weight(z,ips=2,xk=1.5477)*z*dnorm(z)}
Psi1.n    <- function(z,u) {tmp <- z;   ind <- (abs(z)>=u); tmp[ind] <- 0; tmp}
Psi2.n    <- function(z,u) {tmp <- z^2; ind <- (abs(z)>=u); tmp[ind] <- 0; tmp}

Alpha.n <- function(u){2*pnorm(u)-1}
Beta.n  <- function(u){Alfa <- 2*pnorm(u)-1; 2*(-u*dnorm(u)+pnorm(u)-0.5)/Alfa}

I.alpha.n <- function(z0,u,sigma,IS0) {
eta <- -log(dnorm(u)); rh0 <- -log(dnorm(z0))
tmp <- 2*u*dnorm(u)*IS0/sigma+1*(eta>=rh0)-(2*pnorm(u)-1); tmp}

D1.n <- function(u,sigma,IT0,IS0,XtX,xbar){l <- -u
tmp1 <- (u-l)*XtX%*%IT0
tmp2 <- (u^2-l^2)*IS0*xbar
dnorm(u)*(tmp1+tmp2)/sigma}

D2.n <- function(u,sigma,IT0,IS0,xbar){l <- -u 
tmp1  <- (u^2-l^2)*sum(xbar*IT0)
tmp2  <- (u^3-l^3)*IS0
dnorm(u)*(tmp1+tmp2)/sigma}

invM2.n <- function(u,theta,sigma,rs,wi,XtX,xbar,estim=c("SA","TMLA","SI","TMLI")) {
p <- ncol(XtX); n <- length(rs); xk <- 1.5477
if (estim=="SA")  {zpsp <- psp.weight(rs,ips=2,xk=xk)
                   a1c  <- sum(zpsp)/(n*sigma)
                   b1c  <- sum(zpsp*rs)/(n*sigma)
                   zpsi <- psi.weight(rs,ips=2,xk=xk)
                   a2c  <- sum(zpsi)/(n*sigma)
                   b2c  <- sum(zpsi*rs)/(n*sigma)}
if (estim=="TMLA"){a1c  <- sum(wi)/(n*sigma)
                   b1c  <- sum(rs*wi)/(n*sigma)
                   a2c  <- 2*b1c
                   b2c  <- 2*sum(rs^2*wi)/(n*sigma)}
if (estim=="SI")  {a1c   <- integrate(Pspphi.n, lower=-xk,upper=xk, ips=2, xk=xk)$integral/sigma
                   b2c   <- integrate(Psizphi.n,lower=-xk,upper=xk, ips=2, xk=xk)$integral/sigma
                   a2c   <- 0; b1c <- 0}
if (estim=="TMLI"){a1c   <- (2*pnorm(u)-1)/sigma
                   b2c   <- 4*(-u*dnorm(u)+pnorm(u)-0.5)/sigma
                   a2c   <- 0; b1c <- 0}
A1 <- a1c*XtX;    b1 <- b1c*xbar
a2 <- a2c*xbar;   b2 <- b2c
M  <- matrix(0,ncol=p+1,nrow=p+1)
M[1:p,1:p] <- A1; M[1:p,p+1] <- as.matrix(b1)
M[p+1,1:p] <- a2; M[p+1,p+1] <- b2
Minv <- solve(M)
list(Minv=Minv)} 

Cov2.n <- function(X,y,u,theta,sigma,opt=c("integrals","averages")){
xk <- 1.5477
n <- nrow(X); np <- ncol(X); n2 <- n*(n-np)
AV0 <- matrix(0,ncol=np+1,nrow=np+1)
AV1 <- matrix(0,ncol=np+1,nrow=np+1)
rs    <- as.vector(y-X%*%theta)/sigma
wi    <- (abs(rs)<u)*1
sumwi <- sum(wi)
XtX   <- (t(X)%*%diag(wi)%*%X)/sumwi
xbar  <- colMeans(wi*X)*(n/sumwi) 
#alfa <- Alpha.n(u); beta <- Beta.n(u)
if (opt=="averages")  {invM0 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="SA")$Minv 
                       invM1 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="TMLA")$Minv}
if (opt=="integrals") {invM0 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="SI")$Minv 
                       invM1 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="TMLI")$Minv}
ncov  <- max(np*(np+1)/2,2)
avts0 <- matrix(double(1),np+1,np+1)
avts  <- matrix(double(1),np+1,np+1)
f.res_.Fortran("av_tmlnf",X=as.double(X),y=as.double(y),n=as.integer(n),
   np=as.integer(np),ncov=as.integer(ncov),u=as.double(u),k0=as.double(xk),
   theta=as.double(theta),sigma=as.double(sigma),invm0=as.double(invM0),
   invm1=as.double(invM1),avts0=as.double(avts0),avts=as.double(avts),
   xbar=as.double(xbar),XtX=as.double(XtX),sa=double(ncov),sc1=double(ncov),
   x0=double(np),its0=double(np+1),its=double(np+1))
 AV.TS0 <- matrix(f.res$avts0,nrow=np+1,ncol=np+1)
 AV.TS  <- matrix(f.res$avts,nrow=np+1,ncol=np+1)
list(CV0=AV.TS0, CV1=AV.TS, XtX=XtX, xbar=xbar)}

Kov2.n <- function(X,y,u,theta,sigma,opt=c("integrals","averages")){
n <- nrow(X); np <- ncol(X); n2 <- n*(n-np)
AV0 <- matrix(0,ncol=np+1,nrow=np+1)
AV1 <- matrix(0,ncol=np+1,nrow=np+1)
rs    <- as.vector(y-X%*%theta)/sigma
wi    <- (abs(rs)<u)*1
sumwi <- sum(wi)
XtX   <- (t(X)%*%diag(wi)%*%X)/sumwi
xbar  <- colMeans(wi*X)*(n/sumwi) 
alfa  <- Alpha.n(u); beta <- Beta.n(u)
if (opt=="averages")  {invM0 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="SA")$Minv 
                       invM1 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="TMLA")$Minv}
if (opt=="integrals") {invM0 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="SI")$Minv 
                       invM1 <- invM2.n(u,theta,sigma,rs,wi,XtX,xbar,estim="TMLI")$Minv}
for(i in 1:n) {
  x0  <- X[i,]; y0 <- y[i]
# S-estimate
  z0  <- (y0-sum(x0*theta))/sigma
  c1  <- psi.weight(z0,ips=2,xk=1.5477)*x0
  c2  <- chi.weight(z0,ips=2,xk=1.5477)-0.5
  i0  <- invM0%*%as.matrix(c(c1, c2))
  IT0 <- i0[1:np]
  IS0 <- i0[np+1]  
# TML-estimate
  z0   <- (y0-sum(x0*theta))/sigma
  Ialf <- I.alpha.n(z0,u,sigma,IS0)
  tmp1 <- Psi1.n(z0,u)*x0 + D1.n(u,sigma,IT0,IS0,XtX,xbar)
  tmp2 <- Psi2.n(z0,u) - alfa*beta + D2.n(u,sigma,IT0,IS0,xbar) - beta*Ialf
  i1   <- invM1%*%as.matrix(c(tmp1, tmp2))
  IT   <- i1[1:np]
  IS   <- i1[np+1]  
# As.cov estimate
  AV0   <- AV0 + i0 %*% t(i0)
  AV1   <- AV1 + i1 %*% t(i1)}
list(CV0=AV0/n2, CV1=AV1/n2, XtX=XtX, xbar=xbar)}


