rawdata <- read.csv("/Users/mtnorton/Dropbox/temp/HeatIndex/medtemp.csv")

# up to 32 years of data at 59 stations, matrix of 59x32

indexes <- matrix(NA, 59, 32)
total_missing <- matrix(365, 59, 32)

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
      num_data <- length(which(mo_data!=-99))
      # Look up # expected to be missing by month
      exp_missing <- mo_exp_missing[month]
      if (rawdata[i,3]%%4==0 & month==2) {exp_missing <- exp_missing-1} 
      
      total_missing[sta_count,ind_year] <- total_missing[sta_count,ind_year]-num_data
      
      if (num_missing == 31)
      { 
        indexes[sta_count, ind_year] <- NA
        init <- TRUE
      }
      else
      {
        num_missing <- num_missing - exp_missing
      
      #if (num_missing > exp_missing+3) 
      #  {too_many_missing <- TRUE}
      #else
      #{
        # Replace -99 with NA
        mo_data <- replace(mo_data,mo_data==-99,NA)
        
        # Subtract 10 from the total
        mo_data <- mo_data-10
        mo_data_sum <- sum(mo_data,na.rm=TRUE)
        if (num_missing > 1 ) 
        {
          #mo_data<- na.omit(mo_data)
          mo_data_mean <- mean(as.numeric(mo_data),na.rm=TRUE)
          mo_data_sum <- mo_data_sum + num_missing * mo_data_mean
        }
        indexes[sta_count, ind_year] <- sum(c(indexes[sta_count, ind_year], mo_data_sum),na.rm=TRUE)

        print(paste(i, month, year, ind_year, mo_data_sum, mo_data_mean, indexes[sta_count, ind_year]))
      }
      
  }
  
}

stations <- as.matrix(read.csv("/Users/mtnorton/Dropbox/temp/Distance measures/coffeestations.csv"))

sta_output <- matrix(NA, 59, 4)
sta_names_govt <- vector("character",59)

plot(rep(1,32),indexes[1,],col='white',ylim=c(0000,7000),xlim=c(0,2500),xlab="ELEVATION",ylab="Heat Index")

for (i in 1:59)
{
  sta_output[i,1:4] <- cbind(stations[which(stations[,3]==unique(rawdata[,2])[i]/10),c(2,7:9)])
  
  sta_names_govt[i] <- stations[which(stations[,3]==unique(rawdata[,2])[i]/10),4]
  
  # Detrend for elevation with simple line
  # Not mathematical b/c of all the outliers
  # Basic formula: y = -0.5 x + 6700
  # Elevations below 3350/2 = 1675, subtract thermal units
  # Elevations above 3350/2 = 1675, subtract thermal units
  
  elev_trend[i] <- (as.numeric(sta_output[i,2])-1675)*2
  index_detrended[i,] <- indexes[i,]+elev_trend[i]
  
  points(rep(sta_output[i,2],32),indexes[i,]) #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
}

######################
# PLOT detrended data by latitude
######################

plot(rep(1,32),indexes[1,],col='white',ylim=c(0000,7000),xlim=c(0,9),xlab="LATITUDE",ylab="Heat Index")
for (i in 1:59)
{
  points(rep(as.numeric(sta_output[i,3]),32),index_detrended[i,])
}
  
output <- cbind(unique(rawdata[,2]),indexes,sta_output)

write.csv(output,"/Users/mtnorton/Dropbox/temp/HeatIndex/HeatIndex.csv")

######################
# Station Coordinates
######################

sta_coords <- matrix(NA,59,2)
sta_coords[,1] <- as.vector(as.numeric(sta_output[,4]))
sta_coords[,2] <- as.vector(as.numeric(sta_output[,3]))
S <- SpatialPoints(sta_coords)

######################
# GEOSTATISTICS
######################

plot(c(1,1),col='white',xlim=c(0,300000),ylim=c(0,1200000))
models <- matrix(NA, 300, 32)
models[,1] <- (1:300)*1000

for (i in 1:32)
{
  heat <- as.vector(index_detrended[,i])
  if (length(which(is.na(heat))) > 0) {
    coo <- sta_coords[-which(is.na(heat)),]
    heat <- heat[-which(is.na(heat))]    
  } else {coo <- coords}
  coo <- project(coo, "+proj=utm +zone=15 ellps=WGS84")
  heatdf <- data.frame(heat)
  coordinates(heatdf) <- coo
  lzn = variogram(heat~1, heatdf)
  
  lzn.fit = fit.variogram(lzn, model = vgm(psill=max(lzn$gamma), "Sph", range=300000,nugget=0))
  
  #plot(lzn, lzn.fit)
  
  if (lzn.fit$psill[1]/lzn.fit$psill[2]<0.8)
  {
    models[,i+1] <- (1.5*models[,1]/lzn.fit$range[2]-0.5*(models[,1]/lzn.fit$range[2])^3)*lzn.fit$psill[2]+lzn.fit$psill[1]
    models[which(models[,1]>lzn.fit$range[2]),i+1] <- lzn.fit$psill[1] + lzn.fit$psill[2]
    if (i>1) {lines(models[1:300,cbind(1,i)])}
  }
  
}


mean_sd <- matrix(NA, 300, 4)
mean_sd [,1] <- rowMeans(models[,-1],na.rm=TRUE)
for (i in 1:300)
{
  mean_sd[i,2] <- sd(models[i,-1],na.rm=TRUE)
}

mean_sd[,3] <- mean_sd[,1] + 1.96 * mean_sd[,2]
mean_sd[,4] <- mean_sd[,1] - 1.96 * mean_sd[,2]

#Again, blank plot for lines
plot(3343969,1683021,col='white',xlim=c(0,300000),ylim=c(0,1200000),xlab="Distance (m)",ylab="Semivariance")
lines(models[,1],mean_sd[,1])
lines(models[,1],mean_sd[,3],lty=22)
lines(models[,1],mean_sd[,4],lty=22)

