library(tidyverse)
library(naniar)
library(geometry)


#lag function ----

lag_func <- function (x, p) {
  nr <- dim(x)[1]
  xlag<-matrix(data=NA, dim(x)[1], dim(x)[2])
  if (p>=0){
    xlag[(p+1):nr,] = x[1:(nr-p),]
  } else {
    xlag[1:(nr+p),] = x[(1-p):nr,]
  }
  return(xlag)
} 


# hac_tsls function ----


# other funtion ----

