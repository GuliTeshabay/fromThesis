guli<-matrix(rep(0, 48), 24, 24)
nma<-24
ikern<-1
for (i in 1:nma){
  for (j in 1:nma){
    if ( (abs(i-j)) <= nma ){
      guli[i,j]= 1-(abs(i-j))/(nma+1)
    }
  }
}



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

guli2<-kern%*% t(kern)



hac_tsls<- function (y, x, w) {
  x_hat<- w %*% ((inv(t(w) %*% w )) %*% (t(w) %*% x))
  xxi<-inv(t(x_hat)%*%x_hat)
  b_hat<- (xxi)%*%t(x_hat)%*%y
  u <- y-x %*% b_hat
  urep<-matrix(rep(xhat, dim(xhat)[2]), ncol=dim(xhat)[2], byrow=FALSE)
  z<- c(x_hat) * urep
  v<-matrix(0, ncol=dim(xhat)[2], nrow=dim(xhat)[2])
  
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
    v=v+kern[abs(ii)+1,1] %*% (t(z[r1:r2,]) %*% z[(r1-ii):(r2-ii),])
  }
  
}



