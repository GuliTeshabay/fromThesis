#' Deletes all the rows of x that contain a missing element (NA)

packr<-function(a){
  a1<- is.na(a)*1 
  a2<-rowSums(a1)==0
  a2[a2=='TRUE']<-1
  new<-a[a2==1, ]
}


