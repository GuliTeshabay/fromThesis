#Compute number of observations from input dates

nobs<- function (nfirst,nlast,nper) {
  first_year <- nfirst[1]
  first_period<- nfirst[2]
  last_year<- nlast[1]
  last_period <- nlast[2]
  nobs <- nper*(last_year-first_year-1)+last_period+(nper+1-first_period)
}

