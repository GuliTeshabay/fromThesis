


varest<- function (y,var_par,smpl_par){
  # Extract parameters
  nlag <- unlist(var_par[1])
  icomp  <- unlist(var_par[2])
  iconst <- unlist(var_par[3])  
  #Set Up VAR
  ns<-dim(y)[2]
  T<- dim(y)[1]
  x<-cbind( matrix(1, T, 1), lagmatrix(y, 1:nlag))
  if (iconst == 0){
    x=x[,-1] #Eliminate Constant if i_const == 0
  }
  tmp<-smpl_HO(smpl_par)
  istart<-unlist(tmp[1])
  iend<-unlist(tmp[2])
  istart<-max(istart, 1)
  iend<-min(iend, dim(y)[1])
  
  trnd<-matrix(1:T)
  trnd<-trnd[istart:iend] #WARNING MIGHT NEED AS.MATRIX TRANSFORMATION 
  
  y<-y[istart:iend,]
  x<-x[istart:iend,]
  tmp<-cbind(y,x,trnd)
  tmp <- packr(tmp)
  
  y<-tmp[, 1:ns]
  x<-tmp[, (ns+1):(dim(tmp)[2]-1)]
  trnd<-tmp[, (dim(tmp)[2])] #WARNING MIGHT NEED AS.MATRIX TRANSFORMATION 
  
  xx<- t(x)%*%x
  xxi<- inv(xx)
  xy<-t(x) %*% y
  betahat<-xxi%*% xy
  e<- y-x%*%betahat
  ndf<-dim(x)[1]-dim(x)[2]
  seps<-(t(e)%*%e)/ndf
  resid<-matrix(NA, nrow=T, ncol=ns)
  resid[trnd,]=e
  


  if (icomp > 0) {
    #    Transform the VAR so that it is written in Standard form as:
    #    s(t)=P1*s(t-1) + P2*s(t-2) + ... + Pvarlag*s(t-varlag) + eps(t)
    #  
    
    #  Calculate Companion Matrix
    if (iconst==0){
        b = t(betahat)
        const_coef <- matrix(0, nrow=ns, ncol=1)
    } else if (iconst == 1){
        b= t(betahat[2:dim(betahat)[1],])
        const_coef = as.matrix(betahat[1,])     # Coefficients on Constant Term ;
    } else {
        print('Invalid value of i_const in VAREST')
    } # end of the if (iconst==0) condition chain 
    
    comp<-matrix(0, nrow=dim(b)[2], ncol=dim(b)[2])
    comp[1:dim(b)[1],]=b
    if (dim(b)[2]>dim(b)[1]){
      comp[(dim(b)[1]+1):dim(comp)[1],1:(dim(comp)[2]-dim(b)[1])]=diag(dim(b)[2]-dim(b)[1])
    }
    #write model in SS form 
    # y(t) = Q*z(t)
    # z(t) = M*z(t-1) + G*u(t)
    #var(u(t)) = I
    M=comp
    Q<-matrix(0, ns, dim(M)[1])
    Q[1:ns, 1:ns]=diag(ns)
    G<-matrix(0, nrow=dim(M)[1], ns)
    G[1:ns, 1:ns]=t(chol(seps))
    
    
  } #end of if (icomp>0) condition 
  
  if (icomp==2){ #add constant term 
    G<-rbind(G, matrix(0, nrow=1, ncol=dim(G)[2]) )
    Q<-cbind(Q, matrix(0, nrow=dim(Q)[1], ncol=1))
    M<-cbind(M, matrix(0, nrow=dim(M)[1], ncol=1))
    M<-rbind(M, matrix(0, nrow=1, ncol=dim(M)[2]))
    M[dim(M)[1], dim(M)[2]]=1
    M[1:ns, dim(M)[2]]=const_coef 
  } #end of if (icomp==2) condition 
  
  betahat_dim<-dim(betahat)
  seps_dim<-dim(seps)
  resid_dim<-dim(resid)
  Q_dim<-dim(Q)
  M_dim<-dim(M)
  G_dim<-dim(G)
  varout<-list(betahat_dim, betahat, seps_dim, seps, resid_dim, resid,  Q_dim, Q, M_dim, M,  G_dim , G )
  return(varout)
  
  
} #end of the function varest 






