smpl_HO<-function(smpl_par){
  calvec<-as.matrix(unlist(smpl_par[1]))
  nper<-unlist(smpl_par[2])
  nfirst<-as.matrix(unlist(smpl_par[3]))
  nlast<-as.matrix(unlist(smpl_par[4]))
  i1<-nfirst[1]+(nfirst[2]-1)/nper
  i2<-nlast[1]+(nlast[2]-1)/nper
  istart<-which(calvec>(i1-0.0001))[1]
  iend<-which(calvec<(i2+0.0001))[length(which(calvec<(i2+0.0001)))]
  
}

