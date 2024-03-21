# -- Adjust for outliers using fraction of IQR
#      
#       y = Data series
#       thr = threshold in multiples of IQR
#       tflag = 0  == replace with missing value 
#               1  == replace with maximum or minimum value
#               2  == replace with median value
#               3  == replace with local median (obs + or - 3 on each side)
#               4  == replace with one-sided median (5 preceding obs)



adjout <- function (y,thr,tflag) {
  small<-1*((10)^(-6)) #small=1.0e-06
  
  # -- Compute IQR
  z <- as.matrix(packr(y))
  pct_vec <-c(0.25, 0.50, 0.75)
  tmp <- pctile(z,pct_vec)
  zm<-tmp[2]
  iqr<- tmp[3]-tmp[1]
  if (iqr<small){
    x<- matrix(NA, nrow = dim(y)[1])
  }
  if (iqr>= small){
    ya=abs(u-zm)
    
    iya<- ya > (thr*iqr)
    iya[is.na(iya)] <- FALSE 
    iya<-iya*1
    
    iyb <- ya <= (thr*iqr)
    iyb[is.na(iyb)] <- FALSE 
    iyb<-iyb*1
    
    
    
    
  }# end of the if (iqr>= small) statement 
  
  
} #end of the function 



