library(tidyverse)
library(naniar)

#adding standard errors
setwd("C:/Users/nasir/Desktop/LAST SEMESTER/Thesis/empirical")
data_csv<-read.table("C:/Users/nasir/Desktop/LAST SEMESTER/Thesis/empirical/for_tsls.csv", header = T, sep = ",")




b_hat<- ((t(xhat)%*%xhat)^(-1))%*%t(xhat)%*%y

hac_tsls<- function (y, x, w) {
  x_hat <- w %*% (((t(w)%*%w)^(-1))%*%t(w)%*%x)
  b_hat<- (inv(t(x_hat)%*%x_hat))%*%t(x_hat)%*%y
  u<- (y-(x%*% b_hat))
  
  z<-x_hat * matrix(rep(u, dim(x)[2]), dim(u)[1], dim(x)[2], byrow=FALSE)
  v<-matrix(rep(0, 2*dim(x)[2]), dim(x)[2],dim(x)[2])
  
  kern<-matrix(rep(0, (nma+1)), (nma+1), 1)
  for (ii in 0:nma){
    kern[ii+1,]=1
    if (nma > 0){
      if (ikern == 1){
        kern[ii+1, 1]= 1-((ii)/(nma+1))
      }
    }
  }
}


#z = xhat.*repmat(u,1,size(x,2));
z<-rep(kern, 2)
