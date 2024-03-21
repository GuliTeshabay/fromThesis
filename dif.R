# Form x(t) - x(t-p) with missing values for initial conditions 
dif<-function (x,p){
  nr<-dim(x)[1]
  dx<-matrix(NA, nrow=dim(x)[1], ncol=dim(x)[2])
  dx[(p+1):nr,]=x[(p+1):nr,]-x[1:(nr-p),]
  return(dx)





