# TMLw.s
 
# Main function
#--------------

TML.logweibull <- function(X,y,cu=1.855356,initial=c("S","input"),otp=c("adaptive","fixed"),
                  cov=c("no","parametric","nonparametric"),input=NULL,iv=1,
                  nrep=0,seed=1313,maxit=100,tol=0.0001,gam=0.1,nitmon=F){
#
# Main function for TML-estimates; Log-Weibull case
#         
n   <- length(y); np <- ncol(X); cl <- Izero(cu)
ips <- 2; xk <- 1.717817; beta <- 0.5
# Step 1: initial high bdp estimate
if (initial=="S")     {if (np <= 2 & n <= 500) iopt <- 3 else iopt <- 1; if (nrep!=0) iopt <- 2
                       z   <- S.hysest(X,y,iopt,nrep,np+1,ips,xk,beta,time=F,seed=seed)
                       th0 <- z$theta; v0  <- z$smin}
if (initial=="input") {z  <- input; v0 <- z$v; th0 <- z$tau}
namat <- matrix(NA,nrow=np,ncol=np)
nares <- list(th0=th0,v0=v0,th1=rep(NA,np),v1=NA,tl=NA,tu=NA,CV0=namat,V0=NA,CV1=namat,V1=NA,
         alpha=NA,tn=NA,beta=NA,wi=rep(NA,n))
# Step 2: rejection rule
re    <- as.vector(y-X%*%th0); rs <- re/v0
tp    <- adaptw(sort(rs),cl,cu,otp,maxit,tol); if (is.na(tp$tu)) return(nares)
wi    <- tPsiw(rs,tp$tl,tp$tu)
yr    <- y[wi!=0 | rs==0]
Xr    <- X[wi!=0 | rs==0,,drop=F]
tp$tn <- length(yr)
# Step 3: ML-estimate on retained observations
z     <- MLwp(Xr,yr,th0,v0,iv,n,tp,gamm=gam,maxit,tol,nitmon)
res   <- list(th0=th0,v0=v0,th1=z$th1,v1=z$v1,nit=z$nit,tl=tp$tl,tu=tp$tu,
         alpha=tp$alpha,tn=tp$tn,beta=tp$beta,wi=(wi!=0)*1)
if (cov!="no") {l <- cl; u <- cu; if (otp=="adaptive") {l <- tp$tl; u <- tp$tu} 
  if (cov=="parametric"   ) K <- Cov2.w(X,y,l,u,z$th1,z$v1,opt="integrals")
  if (cov=="nonparametric") K <- Cov2.w(X,y,l,u,z$th1,z$v1,opt="averages")
  res  <- c(res,list(CV0=K$CV0,CV1=K$CV1))}
res} 

# Auxiliary functions for TML-estimation
#------------------------------------------------------------------------------

MLwp <- function(Xr,yr,th0,v0,iv,n,tp,gamm,maxit,tol,nitmon){
# Maximum likelihood
th   <- th0; v <- v0; nit <- 1; dv <-0; np <- ncol(Xr); delta <- rep(0,np); tn <- length(yr)
beta <- tp$beta; tn <- tp$tn
repeat {
  rr  <- as.vector(yr-Xr%*%th)
# scale step
  vo <- v 
  if (iv==1) v <- Scalew(vo,rr,tn-np,beta, tol/10,maxit)
  dv <- v-vo
# coefficient step
  rs  <- wi <- rr/v; cnd <- rs!= 0
  wi[cnd] <- tPsiw(rs[cnd],-Inf,Inf)/rs[cnd]
  sqw    <- sqrt(wi)
  rs     <- sqw*rs
  WX     <- sqw*Xr
  XW2X   <- t(WX)%*%WX
  XWr    <- t(WX)%*%rs
  delta  <- solve(XW2X,XWr)
  th     <- th+gamm*as.vector(delta)
  if (nitmon) cat("nit,v,th : ",nit,round(v,4),"\n",round(th,4),"\n")
  if (nit==maxit) cat("MLwp: nit=maxit","\n")
  if (nit==maxit | (all(abs(delta)<tol) & abs(dv)<tol)) break
  nit    <- nit+1}
list(th1=th,v1=v,nit=nit)}

F0w.s <- function(u) {
# Fo+(u) (the function adaptw uses a Fortran version)
if (u > 20) return(1)
if (u <=1) z <- 0 else
 {if (u > 1.5) Tl <- uniroot(rhow,lower=-u,upper=-u+1.5,const=u)$root else
               Tl <- uniroot(rhow,lower=-u,upper=0,const=u)$root 
               Tu <- uniroot(rhow,lower=log(u),upper=u,const=u)$root
z <- pweibull(exp(Tu),shape=1)-pweibull(exp(Tl),shape=1)}; z}

F0w.s <- function(u) {
if (u > 20) return(1)
if (u <=1) z <- 0 else
 {if (u > 1.5) Tl <- uniroot(rhow,lower=-u,upper=-u+1.5,const=u)$root else
               Tl <- uniroot(rhow,lower=-u,upper=0,const=u)$root 
               Tu <- uniroot(rhow,lower=log(u),upper=u,const=u)$root
z <- pweibull(exp(Tu),shape=1)-pweibull(exp(Tl),shape=1)}; z}

F0w <- function(u,tol=0.0001,maxit=150) {# Fortran version of F0w.s
z  <- .Fortran("s_f0w",u=as.double(u),tol=as.double(tol),maxit=as.integer(maxit),p=double(1))
z$p}

zez       <- function(z,const){exp(z)*const-z}
ezez      <- function(z){exp(z-exp(z))}
pezez     <- function(z) {1-exp(-exp(z))}
rhow      <- function(z,const){exp(z)-z-const}

tutl <- function(p){
# p-quantile of standard log-Weibull distribution
if (p>1 | p<0) {warning("Argument out of range\n"); return(0)}
log(-log(1-p))}

Izero <- function(tt) {
# Gives tl for given tt=tu or tu for given tt=tl
if (tt > 3.81) return(-Inf)
const <- ezez(tt)
if (const>=exp(-1)) {warning(paste("No solution for tu =",tu,"\n"));return(tu)}
if (exp(tt)>-log(const)) {up  <- -log(const); low <- -3*abs(up)} else 
                         {low <- -log(const); up  <- 3*abs(low)}
zz <- uniroot(zez,lower=low,upper=up,const=const)
log(zz$root)}

tPsiw <- function(r,tl,tu){
nr   <- length(r); tmp  <- rep(0,nr)
ind  <- (1:nr)[r > tl & r < tu] 
tmp[ind] <- exp(r[ind])-1; tmp}

tChiw <- function(r,tl,tu){
nr   <- length(r); tmp  <- rep(0,nr)
ind  <- (1:nr)[r> tl & r < tu] 
tmp[ind] <- r[ind]*(exp(r[ind])-1); tmp}

tChiww <- function(r,args){
tl <- args$tl; tu <- args$tu
tChiw(r,tl,tu)*exp(r-exp(r))}

AveS20w <- function(v,args){
Beta <- args$Beta; n <- args$n
nr   <- length(args$rr); nv <- length(v); A <- rep(0,nv)
for (i in 1:nv) {
rs   <- args$rr/v[i] 
tmp  <- rs*(exp(rs)-1)
A[i] <- sum(tmp)/n-Beta}; A}

AveS2Pw <- function(v,args){
Beta <- args$Beta; n <- args$n
nr <- length(args$rr); nv <- length(v); A <- rep(0,nv)
for (i in 1:nv) {
rs   <- args$rr/v[i]
tmp  <- rs*exp(rs)*(1+rs)-rs
A[i] <- -(sum(tmp)/v[i])/n}; A}

AveS21w <- function(v,args){
Beta <- args$Beta; n <- args$n
nr   <- length(args$rr); nv <- length(v); A <- rep(0,nv)
for (i in 1:nv) {
rs   <- args$rr/v[i] 
tmp  <- rs*(exp(rs)-1)
A[i] <- sum(tmp)/n/Beta}; A}

Fxdv <- function(v0,rr,n,Beta,tol,maxit){
# Fixed point algorithm for scale
args <- list(rr=rr,n=n,Beta=Beta)
v    <- v0; nit  <- 1
repeat{
  vo <- v
  v  <- AveS21w(vo,args)*vo; d <- v-vo
  if (nit==maxit | abs(d)<tol) break
  nit <- nit+1}
list(v=v,nit=nit,delta=d)}

Nwtv <- function(v0,rr,n,Beta,tol,maxit){
# Newton algorithm for scale
args <- list(rr=rr,n=n,Beta=Beta)
v    <- v0; nit  <- 1
repeat{
  f  <- AveS20w(v,args)
  f1 <- AveS2Pw(v,args)
  if (!is.finite(f) | !is.finite(f1)) {d <- NA; v <- NA; break}
  d  <- -f/f1
  if (is.nan(d) | !is.finite(d)) {d <- NA; v <- NA; break}
  v  <- v+d
# cat(nit,round(c(v,d),5),"\n")
  if (v<=0) {d <- NA; v <- NA; break}
  if (nit==maxit | abs(d)<tol) break
  nit <- nit+1}
list(v=v,nit=nit,delta=d)}

Discr <- function(yo,Po,cu){
# minimum ratio between Fn and Po (for argument > cu)
n  <- length(yo)
# if (cu > yo[n]) {cat("cu is too large ","\n"); return(list(j=NA))}
Fn <- (0:(n-1))/n; r  <- Fn/Po
j  <- (1:n)[yo > cu]
if (length(j)>0) {j <- j[1]; rj <- r[j:n]} else {j <- n+1; rj <- 1}
r.min <- min(rj,1)
if (r.min < 1) j.min <- (j:n)[rj==r.min] else j.min <- NULL
list(j=j,j.min=j.min,r.min=r.min)}

intmew <- function(iopt,tl,tu,til=1e-4) {
b1 <- 0
z  <- .Fortran("s_intmw",iwgt=as.integer(iopt),tl=as.double(tl),tu=as.double(tu),
      b1=as.double(b1),til=as.double(til),sum=double(1))
z$sum}

adaptw <- function(rso,cl,cu,option,maxit,tol){
# adaptive cut-off values
alpha <- NA; n <- length(rso)
if (option=="adaptive"){
 rhos  <- sort(rhow(rso,const=0)); rhocu <- rhow(cu,const=0)
 Po    <- apply(as.matrix(rhos),1,F0w,tol=tol,maxit=150) # F0w is a Fortran version of F0w.s
 dsr   <- Discr(rhos,Po,rhocu)
 alpha <- dsr$r.min
 tu2 <- as.numeric(quantile(rhos,probs=alpha)); logtu2 <- log(tu2); upp <- log(tu2+1.2*logtu2) 
 if (tu2 <=1 )             {tl <- tu <- 0}
 if (tu2>1   & tu2 <=1.5)  tl <- uniroot(rhow,lower=-tu2,  upper=0,       const=tu2)$root
 if (tu2>1.5 & tu2 <= 16)  tl <- uniroot(rhow,lower=-tu2,  upper=-tu2+1.5,const=tu2)$root  
 if (tu2 > 16)             tl <- -tu2
 if (tu2>1   & tu2 <= 50)  tu <- uniroot(rhow,lower=logtu2,upper=tu2,     const=tu2)$root
 if (tu2 > 50)             tu <- uniroot(rhow,lower=logtu2,upper=upp,     const=tu2)$root
 tu <- max(tu,cu); tl <- Izero(tu)}
if (option=="fixed"){tu <- cu; tl <- cl}
tlow <- tl; if (tlow < -16) tlow <- -16
Beta <- integrate(tChiww,tlow,tu,args=list(tl=tl,tu=tu))$integral
beta <- Beta/(pweibull(exp(tu),shape=1)-pweibull(exp(tl),shape=1))
list(cl=cl,cu=cu,tl=tl,tu=tu,alpha=alpha,beta=beta)}

Scalew <- function(vo,rr,den,rhs,tol,maxit){
z  <- Nwtv(vo/4,rr,den,rhs,tol,maxit)$v
if (!is.na(z)) v <- z else v <- Fxdv(vo,rr,den,rhs,tol,300)$v; v}

# Auxiliary functions for covariance matrix of estimates
#------------------------------------------------------------------------------

Pspphi.w  <- function(z) {psp.weight(z,ips=2,xk=1.717817)*ezez(z)}
Pspzphi.w <- function(z) {psp.weight(z,ips=2,xk=1.717817)*z*ezez(z)} 
Psiphi.w  <- function(z) {psi.weight(z,ips=2,xk=1.717817)*ezez(z)}   
Psizphi.w <- function(z) {psi.weight(z,ips=2,xk=1.717817)*z*ezez(z)}

s2phi.w   <- function(z) {z*(exp(z)-1)*ezez(z)}
s1pphi.w  <- function(z) {exp(z)*ezez(z)}
s2pphi.w  <- function(z) {(exp(z)*(1+z)-1)*ezez(z)} 
s1pzphi.w <- function(z) {exp(z)*z*ezez(z)}  
s2pzphi.w <- function(z) {(exp(z)*(1+z)-1)*z*ezez(z)}

Psi1.w    <- function(z,l,u) {tmp <- exp(z)-1;     ind <- (z<l | z>u); tmp[ind] <- 0; tmp}
Psi2.w    <- function(z,l,u) {tmp <- z*(exp(z)-1); ind <- (z<l | z>u); tmp[ind] <- 0; tmp}

Alpha.w <- function(l,u){pezez(u)-pezez(l)}
Beta.w  <- function(l,u){Alfa <- pezez(u)-pezez(l)
           tmp <- integrate(s2phi.w,lower=l,upper=u)$integral; tmp/Alfa}

I.alpha.w <- function(z0,l,u,sigma,IS0) {
eta <- exp(u)-u; rh0 <- exp(z0)-z0
tmp <- (u*ezez(u)-l*ezez(l))*IS0/sigma+1*(eta>=rh0)-(pezez(u)-pezez(l)); tmp}

D1.w <- function(l,u,sigma,IT0,IS0,XtX,xbar){
tmp1 <- (exp(u)-exp(l))*XtX%*%IT0
tmp2 <- (u*exp(u)-u-l*exp(l)+l)*IS0*xbar
ezez(u)*(tmp1+tmp2)/sigma}

D2.w <- function(l,u,sigma,IT0,IS0,xbar){
tmp1 <- (u*(exp(u)-1)-l*(exp(l)-1))*sum(xbar*IT0)
tmp2 <- (u*u*(exp(u)-1)-l*l*(exp(l)-1))*IS0
ezez(u)*(tmp1+tmp2)/sigma}

invM2.w <- function(l,u,theta,sigma,rs,wi,XtX,xbar,estim=c("SA","TMLA","SI","TMLI")) {
p <- ncol(XtX); n <- length(rs); xk <- 1.717817
if (estim=="SA")  {zpsp <- psp.weight(rs,ips=2,xk=1.717817)
                   a1c  <- sum(zpsp)/(n*sigma)
                   b1c  <- sum(zpsp*rs)/(n*sigma)
                   zpsi <- psi.weight(rs,ips=2,xk=1.717817)
                   a2c  <- sum(zpsi)/(n*sigma)
                   b2c  <- sum(zpsi*rs)/(n*sigma)}
if (estim=="TMLA"){sp1 <- exp(rs); sp2 <- (exp(rs)*(1+rs)-1)
                   a1c <- sum(wi*sp1)/(n*sigma)
                   b1c <- sum(wi*sp1*rs)/(n*sigma)
                   a2c <- sum(wi*sp2)/(n*sigma)
                   b2c <- sum(wi*sp2*rs)/(n*sigma)}
if (estim=="SI")  {a1c <- integrate(Pspphi.w, lower=-xk,upper=xk)$integral/sigma
                   a2c <- integrate(Psiphi.w, lower=-xk,upper=xk)$integral/sigma
                   b1c <- integrate(Pspzphi.w,lower=-xk,upper=xk)$integral/sigma
                   b2c <- integrate(Psizphi.w,lower=-xk,upper=xk)$integral/sigma}
if (estim=="TMLI"){a1c <- integrate(s1pphi.w, lower=l,  upper=u)$integral/sigma
                   a2c <- integrate(s2pphi.w, lower=l,  upper=u)$integral/sigma
                   b1c <- integrate(s1pzphi.w,lower=l,  upper=u)$integral/sigma
                   b2c <- integrate(s2pzphi.w,lower=l,  upper=u)$integral/sigma}
A1 <- a1c*XtX;    b1 <- b1c*xbar
a2 <- a2c*xbar;   b2 <- b2c
M  <- matrix(0,ncol=p+1,nrow=p+1)
M[1:p,1:p] <- A1; M[1:p,p+1] <- as.matrix(b1)
M[p+1,1:p] <- a2; M[p+1,p+1] <- b2
Minv <- solve(M)
list(Minv=Minv,XtX=XtX,xbar=xbar)} 

Cov2.w <- function(X,y,l,u,theta,sigma,opt=c("integrals","averages")) {
n <- nrow(X); np <- ncol(X); n2 <- n*(n-np); xk <- 1.717817
AV0 <- matrix(0,ncol=np+1,nrow=np+1)
AV1 <- matrix(0,ncol=np+1,nrow=np+1)
rs    <- as.vector(y-X%*%as.matrix(theta))/sigma
wi    <- ((l<rs)&(rs<u))*1
sumwi <- sum(wi)
XtX   <- (t(X)%*%diag(wi)%*%X)/sumwi
xbar  <- colMeans(wi*X)*(n/sumwi)
#alfa <- Alpha.w(l,u); beta <- Beta.w(l,u)
if (opt=="averages")  {invM0 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="SA")$Minv 
                       invM1 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="TMLA")$Minv}
if (opt=="integrals") {invM0 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="SI")$Minv 
                       invM1 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="TMLI")$Minv}
ncov  <- max(np*(np+1)/2,2)
avts0 <- matrix(double(1),np+1,np+1)
avts  <- matrix(double(1),np+1,np+1)
 f.res_.Fortran("av_tmlwf",X=as.double(X),y=as.double(y),n=as.integer(n),
   np=as.integer(np),ncov=as.integer(ncov),l=as.double(l),u=as.double(u),
   xk=as.double(xk),theta=as.double(theta),sigma=as.double(sigma),
   invm0=as.double(invM0),invm1=as.double(invM1),avts0=as.double(avts0),
   avts=as.double(avts),xbar=as.double(xbar),XtX=as.double(XtX),
   sa=double(ncov),sc1=double(ncov),x0=double(np),its0=double(np+1),
   its=double(np+1))
 AV.TS0 <- matrix(f.res$avts0,nrow=np+1,ncol=np+1)
 AV.TS  <- matrix(f.res$avts,nrow=np+1,ncol=np+1)
list(CV0=AV.TS0, CV1=AV.TS, XtX=XtX, xbar=xbar)}

Kov2.w <- function(X,y,l,u,theta,sigma,opt=c("integrals","averages")) {
n <- nrow(X); np <- ncol(X); n2 <- n*(n-np)
AV0 <- matrix(0,ncol=np+1,nrow=np+1)
AV1 <- matrix(0,ncol=np+1,nrow=np+1)
rs    <- as.vector(y-X%*%as.matrix(theta))/sigma
wi    <- ((l<rs)&(rs<u))*1
sumwi <- sum(wi)
XtX   <- (t(X)%*%diag(wi)%*%X)/sumwi
xbar  <- colMeans(wi*X)*(n/sumwi)
alfa  <- Alpha.w(l,u); beta <- Beta.w(l,u)
if (opt=="averages")  {invM0 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="SA")$Minv 
                       invM1 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="TMLA")$Minv}
if (opt=="integrals") {invM0 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="SI")$Minv 
                       invM1 <- invM2.w(l,u,theta,sigma,rs,wi,XtX,xbar,estim="TMLI")$Minv}
for (i in 1:n) {
  x0   <- X[i,]; y0 <- y[i]
# S-estimate
  z0   <- (y0-sum(x0*theta))/sigma
  c1   <- psi.weight(z0,ips=2,xk=1.717817)*x0
  c2   <- chi.weight(z0,ips=2,xk=1.717817)-0.5
  i0   <- invM0%*%as.matrix(c(c1,c2))
  IS0  <- i0[np+1]
  IT0  <- i0[1:np]; IT0[1] <- IT0[1]+0.1352*IS0
  i0   <- c(IT0,IS0)
# TML-estimate
  z0   <- (y0-sum(x0*theta))/sigma 
  Ialf <- I.alpha.w(z0,l,u,sigma,IS0)
  tmp1 <- Psi1.w(z0,l,u)*x0 + D1.w(l,u,sigma,IT0,IS0,XtX,xbar)
  tmp2 <- Psi2.w(z0,l,u) - alfa*beta + D2.w(l,u,sigma,IT0,IS0,xbar) - beta*Ialf 
  i1   <- invM1%*%as.matrix(c(tmp1,tmp2))
  IT   <- i1[1:np]
  IS   <- i1[np+1]
# As.cov estimate
  AV0   <- AV0 + i0 %*% t(i0)
  AV1   <- AV1 + i1 %*% t(i1)} 
list(CV0=AV0/n2, CV1=AV1/n2, XtX=XtX, xbar=xbar)}

