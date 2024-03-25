#' Procedure for producing OLS estimates of b
#' based on Homoskedasticity of residuals assumption 
#'Input:
#' y = tx1
#' x = txk
#' Output:
#' Beta = OLS estimate of beta (kx1)
#' VBeta = Estimate of covariance matrix of beta (kxk)


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
  se_beta<- as.matrix(sqrt(diag(vbeta)))
  bhat_dim<-dim(bhat)
  vbeta_dim<-dim(vbeta)
  se_beta_dim<-dim(se_beta)
  return(list(bhat_dim, bhat, vbeta_dim, vbeta, se_beta_dim, se_beta, ser, rbarsq))
}
