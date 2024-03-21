
smpl<- function (calvec, nfirst, nlast, nper){
  i1<-nfirst[1]+(nfirst[2]-1)/nper
  i2<-nlast[1]+(nlast[2]-1)/nper
  tmp1<-(calvec> (i1-0.0001))*1
  tmp2<-(calvec< (i2+0.0001))*1
  ismpl <- tmp1 * tmp2
}