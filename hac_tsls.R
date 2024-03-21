# hac_tsls function ----


hac_tsls<- function (y, x, w, nma,ikern) {
  x_hat<- w %*% ((inv(t(w) %*% w )) %*% (t(w) %*% x))
  xxi<-inv(t(x_hat)%*%x_hat)
  b_hat<- (xxi)%*%t(x_hat)%*%y
  u <- y-x %*% b_hat
  urep<-matrix(rep(u, dim(x)[2]), ncol=dim(x)[2], byrow=FALSE)
  z<- c(x_hat) * urep
  v<-matrix(0, ncol=dim(x)[2], nrow=dim(x)[2])
  
  
  #form kernel
  kern<-matrix(rep(0, (nma+1)), (nma+1), 1)
  for (ii in 0:nma){
    kern[ii+1,]=1
    if (nma > 0){
      if (ikern == 1){
        kern[ii+1, 1]= 1-((ii)/(nma+1))
      }
    }
  }
  
  #Form Hetero-Serial Correlation Robust Covariance Matrix 
  for (ii in -nma:nma){
    if (ii<=0){
      r1=1 
      r2=dim(z)[1]+ii
    } else {
      r1=1+ii
      r2=dim(z)[1]
    }
    v=v+kern[abs(ii)+1,1] * (t(z[r1:r2,]) %*% z[(r1-ii):(r2-ii),])
  }
  vbeta<- xxi %*% v %*% xxi
  se_beta<-sqrt(diag(vbeta))
  return(list(b_hat, vbeta, se_beta))
}
