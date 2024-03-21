# PACKR
# PACKR(x) deletes all the rows of x that contain a MATLAB missing element (NA)


packr<-function(a){
  a1<- is.na(a)*1 
  a2<-rowSums(a1)==0
  a2[a2=='TRUE']<-1
  new<-a[a2==1, ]
}
### development stage

guli<-matrix(c(NA, 1,2,3,4,5,6, NA, 8,5,9,NA,12,45), 7,2 )
g2<- is.na(guli)*1 
g4<-rowSums(g2)
g4<-g4==0
g4[g4=='TRUE']<-1
new<-guli[g4==1, ]


guli<-matrix(c(NA, 1,2,3,4,5,6, NA, 8,5,9,NA,12,45), 7,2 )
g2<- is.na(guli)*1 
g4<-rowSums(g2)==0
g4[g4=='TRUE']<-1
new<-guli[g4==1, ]
