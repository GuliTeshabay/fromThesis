#' Procedure produces matrix x<-x(y-lag)

lagmatrix<-function (y, lag){
  T<-dim(y)[1]
  tmp<-length(lag)
  nvar<-dim(y)[2]
  result<-matrix(NA, nrow=T, ncol=nvar*tmp)
  
  
  
  for ( j in 1:tmp){
    p=lag[j]
    result[(p+1):T,((j-1)*nvar+1):((j-1)*nvar+nvar)]=y[1:(T-p),]
    
  }
  return(result)
  
} # end of the function lagmatrix
