#input 
# y  is tx1
#x is txk

#Output:
#  Beta = OLS estimate of beta (kx1)
#VBeta = Estimate of covariance matrix of beta (kxk)


hom_ols <- function (y, x){
  xx<- t(x)%*%x
  xxi<- inv(xx)
  xy<-t(x) %*% y
  bhat<-xxi%*% xy
  u<- y-x%*%bhat
  y_m<- y-mean(y)
  tss<-t(y_m)%*%y_m
  ess<- t(u) %*% u
  ndf<- dim(x)[1] - dim(x)[2]
  ser<- sqrt(c(ess)/ndf)
  vbeta<-(c(ess)/ndf)*xxi
  rbarsq<- 1 - (c(ess)/ndf)/(tss/(dim(x)[1]-1))
  se_beta<- sqrt(diag(vbeta))
  return(list(bhat, vbeta, se_beta, ser, rbarsq))
}







