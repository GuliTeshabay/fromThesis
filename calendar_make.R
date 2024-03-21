calendar_make <- function (nfirst,nlast,nper) {
  first_year <- nfirst[1]
  first_period<- nfirst[2]
  last_year<- nlast[1]
  last_period <- nlast[2]
  nobs <- nper*(last_year-first_year-1)+last_period+(nper+1-first_period)
  #calvec<-as.matrix(linspace(first_year+(first_period-1)/nper,last_year+(last_period-1)/nper,nobs))
  calvec<-as.matrix(seq((first_year+(first_period-1)/nper), last_year+(last_period-1)/nper, length.out=nobs))
  calds<-matrix(NA, nobs, 2)
  yr<-first_year
  per<-first_period-1
  for (i in 1:nobs){
    per= per + 1
    if (per>nper){
      per=1
      yr=yr+1
    }
    calds[i, 1]=yr
    calds[i, 2]=per
  }
  return(list(nobs, calvec, calds))
}
### end of function 


calds = NaN*zeros(nobs,2);
yr = first_year;
per = first_period-1;



nobs<- 402
calds<- matrix(NA, 402, 2)
yr <- 1979
first_period<- 1
per<-first_period-1
nper<-12


for (i in 1:nobs){
  per= per + 1
  if (per>nper){
    per=1
    yr=yr+1
  }
  calds[i, 1]=yr
  calds[i, 2]=per
}


calds = NaN*zeros(nobs,2);
yr = first_year;
per = first_period-1;
for i = 1:nobs;
per = per+1;
if (per > nper);
per = 1;
yr = yr + 1;
end;
calds(i,1) = yr;
calds(i,2) = per;