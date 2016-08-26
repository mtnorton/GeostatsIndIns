rawdata <- read.csv("/Users/mtnorton/Dropbox/temp/HeatIndex/medtemp.csv")

# up to 22 years of data at 59 stations, matrix of 59x22

total_missing <- matrix(NA, 59, 32)

#Initialize
sta <- rawdata[1,2]
sta_count <- 1
#too_many_missing <- FALSE
init <- TRUE

# Expected missing per month
mo_exp_missing <- c(0,3,0,1,0,1,0,0,1,0,1,0)

for (i in (1:nrow(rawdata)))
{
  year <- rawdata[i,3]
  month <- rawdata[i,4]
  
  if (rawdata[i,2]!=sta) 
  {
    sta_count <- sta_count+1
    sta <- rawdata[i,2]
    init <- TRUE
  }
  
  if (month > 7) 
  {ind_year <- year-1982}
  else 
  {ind_year <- year-1983}
  
  if (init==TRUE) 
  {if (rawdata[i,4]==7) {init = FALSE}}
  else
  {
    # Make it as own object
    mo_data <- rawdata[i,5:35]
    
    # Count missing
    num_missing <- length(which(mo_data==-99))
    # Look up # expected to be missing by month
    exp_missing <- mo_exp_missing[month]
    if (rawdata[i,3]%%4==0 & month==2) {exp_missing <- exp_missing-1} 
    
    print(paste(i,num_missing,exp_missing))
    
    num_missing <- num_missing - exp_missing
    
    #as matrix
    total_missing[sta_count, ind_year] <- sum(c(total_missing[sta_count, ind_year], num_missing),na.rm=TRUE)
    
  }
  #print(paste(i,num_missing))
}

#as vector
total_days_missing <- as.vector(total_missing)
plot(ecdf(total_days_missing),xlab='Days of missing data',main='CDF of missing days of data',pch=1,ylab="Cumulative Density of Missing Days")
