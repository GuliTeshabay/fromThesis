#transforms x

#   Return Series with same dimension and corresponding dates
#   Missing values where not calculated
#   -- Tcodes:
#            1 Level
#            2 First Difference
#            3 Second Difference
#            4 Log-Level
#            5 Log-First-Difference
#            6 Log-Second-Difference
#            7 dif[(x(t)/x(t-1))-1]

transx<-function(x, tcode){
  small<-1*((10)^(-6)) #small=1.0e-06
  n<- dim(x)[1]
  y<- matrix(NA, n, 1)
  if (tcode==1){
    y=x
  } else if (tcode==2){
    y[2:n]=x[2:n]-x[1:(n-1)]
  } else if (tcode==3){
    y[3:n]=x[3:n]-2*x[2:(n-1)]+x[1:(n-2)]
  } else if (tcode==4){
    if (min(x, na.rm = TRUE)>small){
      x <- log(x)
      y=x
    } else{
      y<-matrix(NA, nrow=n, ncol=1)
    }
  } else if (tcode==5) {
    if (min(x, na.rm = TRUE)>=small){
      x <- log(x)
      y[2:n]=x[2:n]-x[1:(n-1)]
    } else{
      y<-matrix(NA, nrow=n, ncol=1)
    }
  } else if (tcode==6) {
    if (min(x, na.rm = TRUE)>=small){
      x <- log(x)
      y[3:n]=x[3:n]-2*x[2:(n-1)]+x[1:(n-2)]
    } else {
      y<-matrix(NA, nrow=n, ncol=1)
    }
  } else if (tcode==7) {
    tmp<-matrix(NA, nrow=n, ncol=1)
    tmp[2:dim(x)[1]]<-(x[2:dim(x)[1]]/x[1:(dim(x)[1]-1)])-matrix(1, nrow=(dim(x)-1), 1)
    y<-dif(tmp, 1)
  } else {
    y<-matrix(NA, nrow=n, ncol=1)
  }
  return (y)
} #bracket of the function 