#' Procedure for estimating the regression y = xbeta+ u 
#' The procedure produces the OLS estimate of b and 
#' a hetero/autocorrelation consistent estimate of the autocorrelation matrix.

#' Input:
#' y = tx1
#' x = txk
#' nma=truncation parameter (nma=0, White SEs) 
#' ikern = kernel indicator
#' 2 => rectangular

#' Output:
#' Beta = OLS estimate of beta (kx1)
#' VBeta = Robust estimate of covariance matrix of beta (kxk)


hac_ols<- function (y,x,nma,ikern) {
  xx<- t(x)%*%x
  xxi<- inv(xx)
  xy<-t(x) %*% y
  bhat<-xxi%*% xy
  u<- y-x%*%bhat
  
  urep<-matrix(rep(u, dim(x)[2]), ncol=dim(x)[2], byrow=FALSE)
  z<- c(x) * urep
  v<-matrix(0, ncol=dim(x)[2], nrow=dim(x)[2])
  
  
  
  #Form Kernel 
  kern<-matrix(rep(0, (nma+1)), (nma+1), 1)
  for (ii in 0:nma){
    kern[ii+1,]=1
    if (nma > 0){
      if (ikern == 1){
        kern[ii+1, 1]= 1-((ii)/(nma+1))
      }
    }
  }
  # Form Hetero-Serial Correlation Robust Covariance Matrix
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
  vbeta<- xxi %*% v %*% xxi # variance-covariance matrix
  
  # Compute Other Statistics of interest
  y_m<- y- mean(y)
  tss<-t(y_m)%*%y_m
  ess<-t(u)%*%u
  ndf<-dim(x)[1]-dim(x)[2]
  ser <- sqrt(ess/ndf)
  rbarsq <- 1 - (ess/ndf)/(tss/(dim(x)[1]-1))
  se_beta<-sqrt(diag(vbeta)) # standard errors
  
  
  #necessary dimensions for return 
  vbeta_dim<-dim(vbeta)
  return(list(bhat, vbeta_dim, vbeta, se_beta, ser, rbarsq))
}




