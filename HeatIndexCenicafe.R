# Now for Cenicafe

rawdata <- read.csv("/Users/mtnorton/Dropbox/temp/HeatIndex/cenicafe2.csv")

indexes_cc <- matrix(NA, 50, 26)
total_missing_cc <- matrix(NA, 50, 26)

sta_names <- matrix(NA, 50, 3)

#Initialize
sta1 <- rawdata[1,1]
sta2 <- rawdata[1,2]
sta3 <- rawdata[1,3]

sta_names[1,1:3] <- as.matrix(rawdata[1,1:3])

sta_count <- 1
# too_many_missing <- FALSE
# init <- TRUE

# Expected missing per month
month_length <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for (i in (1:nrow(rawdata)))
{
  year <- rawdata[i,4]
  month <- rawdata[i,5]
  
  if (!((rawdata[i,1]==sta1) & (rawdata[i,2]==sta2) & (rawdata[i,3]==sta3)))
  {
    sta_count <- sta_count+1
    sta1 <- rawdata[i,1]
    sta2 <- rawdata[i,2]
    sta3 <- rawdata[i,3]
    sta_names[sta_count,1:3] <- as.matrix(rawdata[i,1:3])
    #init <- TRUE
  }
  
  if (month > 7) 
  {ind_year <- year-1988}
  else 
  {ind_year <- year-1989}
    
  if ((month==8) & (rawdata[i,6]==1))
  {  
    numdays <- 365
    if (year%%4==3) {numdays <- numdays+1}
    # Make it as own object
    mo_data <- rawdata[i:(i+numdays),7]-10
    mo_data <- sum(mo_data,na.rm=TRUE)
    
    # Count missing
    total_missing_cc[sta_count, ind_year] <- length(which(is.na(rawdata[i:(i+numdays),7])))
    
    indexes_cc[sta_count, ind_year] <- mo_data
      
    #i <- i + 365
    #if (rawdata[i,3]%%4==0 & month==2) {i <- i + 1}
    
  }
  print(paste(i, month, year, ind_year, mo_data))
        
}

# delete last year of data
indexes_cc <- indexes_cc[,-26]
total_missing_cc <- total_missing_cc[,-26]
# delete rows 1, 10, 14, 16, 20, 48
indexes_cc <- indexes_cc[-c(1,10,14,16,20,48),]
total_missing_cc <- total_missing_cc[-c(1,10,14,16,20,48),]

stations_cc <- as.matrix(read.csv("/Users/mtnorton/Dropbox/temp/CCData/CCstations_wo_names.csv",header=FALSE))

sta_output_cc <- matrix(NA, 44, 3)

plot(rep(1,25),indexes_cc[1,],col='white',ylim=c(-2000,7000),xlim=c(0,2500),xlab="ELEVATION",ylab="Heat Index")

for (i in 1:44)
{
  sta_output_cc[i,1:3] <- as.matrix(stations_cc[i,c(2,9:10)])
  points(rep(sta_output_cc[i,1],25),indexes_cc[i,],col='red') #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
}

output <- cbind(unique(rawdata[,2]),indexes,sta_output)

write.csv(output,"/Users/mtnorton/Dropbox/temp/HeatIndex/HeatIndex.csv")
