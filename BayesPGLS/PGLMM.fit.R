######################################
## Function calls a PGLMM for logistic regression
## Created by Matthew Helmus
######################################

PGLMM.fit<-function(dat=NULL,Y=NULL,X=NULL,VV=NULL,sp.init=0.5,maxit=25,exitcountermax=50) # [B,B0,s,S95int,LL,flag]=PGLMM_nosparse_funct(Y,X,VV,s)
{
    if (!require(corpcor))
    {
      stop("The 'corpcor' package is required")
    }

  # dat = list from PGLMM.data
  # X = independent variable
  # Y = dependent variable (binary 0,1)
  # VV = list containing covariance matrices
  # s = initial values of s

  # Returns
  # B_SE = coefficients with SEs from GLMM
  # B0_SE = coefficients with SEs from logistic regression
  # s = parameter(s) of the covariance matrix
  # flag = 1 if convergence achieved, 0 otherwise

  #global tH tinvW tVV tX Lmin S iss
  if(!is.null(dat))
  {
    X<-dat$XX
    Y<-dat$YY
    VV<-dat$VV
  }
  
  is.empty<-function(x){length(x) == 0}
  if(any(unlist(lapply(list(X=X,Y=Y,VV=VV),is.empty))))
  {
    stop("a data matrix is empty")
  }

  if(any(unlist(lapply(list(X=X,Y=Y,VV=VV),is.na))))
  {
    stop("a data matrix is NA")
  }

  n<-dim(X)[1]
  p<-dim(X)[2]

  #initial estimates for the s parameters (scaling parameters of the covariance matrices
  sp<-matrix(sp.init,length(as.list(VV)),1)

  B0<-matrix(mean(Y),p,1)
  oldB0<-matrix(10,p,1)

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #% initial values for B
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  counter<-0
  #sparse<-function(W){as(Matrix(W),"sparseMatrix")}
  while (matrix((t(oldB0-B0) %*% (oldB0-B0) > 10^-8)) & (counter<100))
  {
    oldB0<-B0
    counter<-counter+1

    mu<-exp(X %*% B0) / (1+exp(X %*% B0))
    W<-as.vector((mu*(1-mu))^-1)
    invW<-(diag(W))
    #invW<-sparse(diag(W))

    Z<-(X %*% B0 + (Y-mu)/(mu*(1-mu)))
    denom<-(t(X) %*% invW %*% X)
    #Z<-sparse(X %*% B0 + (Y-mu)/(mu*(1-mu)))
    #denom<-sparse(t(X) %*% invW %*% X)

    options(warn=-1)
    if(any(c(is.nan(denom),is.infinite(denom),is.null(denom))))
    {
		  B0<-solve((t(X)%*% X),(t(X) %*% Y))
      (counter<-100)
    } else {
      #num<-sparse(t(X) %*% invW %*% Z)
      num<-(t(X) %*% invW %*% Z)
      B0<-pseudoinverse(denom) %*% num
    }
  }

  if (is.nan(B0) | is.infinite(B0))
  {
	 mu<-matrix(mean(Y),p,1)
	 B0<-log(mu/(1-mu))
  }

  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## GLMM
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # initial estimates
  B<-B0
  b<-matrix(0,n,1)
  #bet<-sparse(rBind(B,b))
  #mu<-sparse(exp(X %*% B)/(1+exp(X %*% B)))
  bet<-(rbind(B,b))
  mu<-(exp(X %*% B)/(1+exp(X %*% B)))  # the mean function for the binomial process change this to get at other distributions

  # matrix including fixed covariates and dummies for "random effects"
  XX<-cbind(X,diag(n))
  Cdum<-matrix(0,n,n)
  for(i in 1:length(sp))
  {
	 Cdum=Cdum+(sp[i]*VV[[i]])
  }
  #Cdum<-Matrix(Cdum)
  est<-t(rbind(sp,B))
  oldest<-matrix(10^6,dim(est)[1],dim(est)[2])

  exitcounter<-0
  while (matrix(((est-oldest) %*% t(est-oldest)) > 10^-4) & exitcounter<=exitcountermax)
  {
    oldest<-est
    # Using notation of Breslow & Clayton (1993)
    # Note: Schall (1991) equation for V incomplete; see McCullagh & Nelder p. 40 and
    # Breslow & Clayton (1993)
    W<-as.vector((mu*(1-mu))^-1)
    #invW<-sparse(diag(W))
    invW<-(diag(W))

    V<-invW+Cdum  #Breslow & Clayton (1993) between eq 10 and 11
    invV<-solve(V,diag(n))  # needed for eq 10

    #################### I did not deal with this code for when the solve function returns a singular matrix  ###################
    #ww=lastwarn;
    #lastwarn('noper')
    #if sum(ww(1:5)=='Matri')==5
		#'Matrix close to singular: alternative B0 tried'
		#mu=rand(length(X(1,:)),1);
		#B0=log(mu./(1-mu));
		#b=zeros(n,1);
		#beta=[B0;b];

		#mu=exp(X*B0)./(1+exp(X*B0));

		#invW=diag((mu.*(1-mu)).^-1);
		#V=invW+C;
		#invV=V\eye(n);

		#B0'
    #end
    #########################################
    
    Z<-X %*% B + b + (Y-mu)/(mu*(1-mu))
    denom<-(t(X) %*% invV %*% X)    # left side of eq 10
    #denom<-sparse(t(X) %*% invW %*% X)
    num<-(t(X) %*% invV %*% Z) #right side of eq 10
    B<-pseudoinverse(denom) %*% num  # solve eq 10 for the fixed effects (alpha in Breslow & Clayton (1993)) 
    b<-Cdum %*% invV %*% (Z-X %*% B) #eq 11

    bet<-(rbind(B,b))
    mu<-exp(XX %*% bet)/(1+exp(XX %*% bet))

    #DEFINE THESE AS GLOBAL tH tinvW tVV tX
    tH<<-Z - X %*% B
    tinvW<<-diag(as.vector((mu*(1-mu))^-1))
    tX<<-X
    tVV<<-VV

    # call to obtain estimates of covariance matrix parameters s
    #options=optimset('MaxFunEvals',25,'MaxIter',25,'TolX',10^-2,'TolFun',10^-2);
    #pars<-list(sp=sp,tVV=tVV,tH=tH,tinvW=tinvW,tVV=tVV,tX=tX)
    sp<-abs(optim(sp,PGLMM.reml,control=list(maxit=maxit,abstol=10^-1))$par)

    if(exitcounter==0){
      scrn.output<-c(exitcounter,t(sp), t(B[1:4]))
      names(scrn.output)<-c("exitcounter",paste("sigma",1:length(sp)),paste("B",1:4))
      print(scrn.output)
    } else {
      print(c(exitcounter,t(sp), t(B[1:4])))
    }
    
    Cdum<-matrix(0,n,n)
    for(i in 1:length(sp))
    {
	     Cdum=Cdum + sp[i] * tVV[[i]]
    }
    est<-t(rbind(sp,B))
    exitcounter<-exitcounter+1
  }

  # flag cases of non-convergence
  flag <- "converged"
  if(exitcounter>=exitcountermax)
  {
	 flag<-"did not converge, try increasing exitcountermax"
  }

  ## flag cases of non-convergence
  #if(is.nan(B)){
	# return
  #}
  
  ##############
  # compute final estimates and SEs
  W<-as.vector((mu*(1-mu))^-1)
  #invW<-sparse(diag(W))
  invW<-(diag(W))
  V<-invW+Cdum
  invV<-solve(V,diag(n))

  Z<-X %*% B + b + (Y-mu)/(mu*(1-mu))
  B<-solve((t(X) %*% invV %*% X),(t(X) %*% invV %*% Z))

  Chi025<-5.0238
  Chi05<-3.8414

  #options=optimset('MaxFunEvals',100,'MaxIter',100,'TolX',5*10^-3,'TolFun',10^-4);

  S95int<-NULL
  S<-sp
  for(iss in 1:length(sp))
  {
    Lmin<-PGLMM.reml(S)
    #Smin
    if(sp[iss] > 0.02)
    {Smin<-optimize(PGLMM.reml,c(0,.9*sp[iss]),tol = 0.0001)$minimum
    } else {
    Smin<-0}
    if(Smin<0.01){Smin<-0}
    #Smax
	   if(sp[iss] > 0.02){
	     Smax<-optimize(PGLMM.reml,c(1.1*sp[iss],10*sp[iss]),tol = 0.0001)$minimum
	   } else {
	     Smax<-optimize(PGLMM.reml,c(.1,5),tol = 0.0001)$minimum}
    S95int<-rbind(S95int,c(abs(Smin),abs(Smax)))
  }
  
  names(sp)<-names(VV)
  colnames(S95int)<-c("0.05","0.95")
  return(list(B=B,B0=B0,s=cbind(sp,S95int),LL=Lmin,flag=flag))
}