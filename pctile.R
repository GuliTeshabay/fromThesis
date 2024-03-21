
pctile<-function (x, pct) {
  x<-matrix(sort(x), dim(x)[1], 1)
  pct<-round(pct*dim(x)[1])
  xpct<-x[pct]
  return(xpct)
}


