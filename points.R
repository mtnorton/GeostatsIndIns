library(sp)
library(raster)
library(geosphere)
library(gstat)

dir <- "/Users/mtnorton/Dropbox/Scholarly Activities/Conferences/2015/AAEA 2015/"

# read in file

zz <- file(paste0(dir,"ghcnd-stations.txt"), 'r')

# parse out lat and long coords
# need lats between 40 to 44, longs between -97 and 90

line <- readLines(zz,n=1)
coords <- matrix(NA,51,2)
elevs <- vector("numeric",51)
filenames <- vector("character",51)
i <- 1

while (length(line)>0)
{
	lat <- as.numeric(substr(line,14,20))
	lon <- as.numeric(substr(line,23,30))
	if ((lat >= 40) && (lat <= 44)  && (lon > -97) && (lon < -90) && (substr(line,1,3)=="USC"))
	{
		# print(line)
		filename <- paste0(dir,"ghcnd_hcn/ghcnd_hcn/",substr(line,1,11),".dly")
		if (file.exists(filename)) 
		{
			print(paste(lat,lon))
			coords[i,2] <- lat
			coords[i,1] <- lon
      elevs[i] <- as.numeric(substr(line,31,37))
			filenames[i] <- filename
      i <- i + 1
		}
	} 
	line <- readLines(zz,n=1)
}

stations <- SpatialPoints(coords)

# Pull in state/county shapefiles

states <- shapefile(paste0(dir,"USA_adm/USA_adm1.shp"))
county <- shapefile(paste0(dir,"USA_adm/USA_adm2.shp"))

# Define box in and around Iowa

boxlimit <- rbind(c(-97,44),c(-90,44),c(-90,40),c(-97,40),c(-97,44))
bbox <- SpatialPolygons(list(Polygons(list(Polygon(boxlimit)),1)))

# Crop state/county files by Iowa box

iowa <- crop(states,bbox)
county <- crop(county,bbox)
crs(stations) <- crs(county)

# Get counties with stations, reorder by GEOID

counties <- over(stations,county)
counties <- cbind(counties, STA=1:51)
counties <- counties[order(counties[,5]),]

# Reorder filenames into same order by GEOID

filenames <- filenames[counties$STA]

#plot(iowa)
#plot(stations, col='red',add=TRUE)


# Now read in Precip for month of June only

# matrix of rainfall totals in June
# 114 years, 51 stations
JunePrecip <- matrix(NA,114,51)

for (i in 1:51)
{
  zz <- file(filenames[i],'r')
  line <- readLines(zz,n=1)
  lineno <- 1
  
  while(length(line)>0)
  {
    yr <- as.numeric(substr(line,12,15))
    if ((substr(line,16,17)=="07") && (substr(line,18,21)=="PRCP") && (yr > 1900) && (yr < 2015))
    {
      # parse line
      total <- 0
      for (day in 1:31)
      {
        startpos <- 14+(day*8)
        val <- as.numeric(substr(line,startpos,startpos+4))
        # make sure it's not -9999
        if (val > 0) {total <- total + val}
      }
      JunePrecip[(yr-1900),i] <- total
    }
    #print(paste("file ",i," line ",lineno))
    line <- readLines(zz,n=1)
    lineno <- lineno + 1
  }
print (i)
}

# Now get Degree Days for June, July, August

strike <- 250 # In tenths of a degree Celsius
DegreeDays <- matrix(NA,114,51)
AvgTemp <- matrix(NA,114,51)

for (i in 1:51)
{
  zz <- file(filenames[i],'r')
  line <- readLines(zz,n=1)
  lineno <- 1
  
  while(length(line)>0)
  {
    yr <- as.numeric(substr(line,12,15))
    if (((substr(line,16,17)=="06") || (substr(line,16,17)=="07") || (substr(line,16,17)=="08")) && (substr(line,18,21)=="TMAX") && (yr > 1900) && (yr < 2015))
    {
      # parse line
      total <- 0
      ndays <- 0
      totaltemp <- 0
      for (day in 1:31)
      {
        startpos <- 14+(day*8)
        val <- as.numeric(substr(line,startpos,startpos+4))
        # make sure it's not -9999, add HDD if temp
        if (val > strike) {total <- total + (val-strike)/10}
        if (val > 0) 
        { # This if statement for AvgTemp
          ndays <- ndays + 1
          totaltemp <- totaltemp + val
        }
      }
      DegreeDays[(yr-1900),i] <- total
      AvgTemp[(yr-1900),i] <-(totaltemp/ndays)/10
    }
    #print(paste("file ",i," line ",lineno))
    line <- readLines(zz,n=1)
    lineno <- lineno + 1
  }
  print (i)
}


###########################
#READ CROP DATA
###########################

cropdata <- read.csv(paste0(dir,"CropYieldData.csv"))

detrended_cropdata <- matrix(NA,35,51)

u <- unique(cropdata[,1])

# Detrend through linear regression

for (i in 1:51)
{
  cd <- cropdata[which(cropdata[,1]==u[i]),4]  
  yr <- cropdata[which(cropdata[,1]==u[i]),2]-1979  
  m <- lm(cd~yr)
  yr <- yr*m$coefficients[2]
  detrended_cropdata[1:length(yr),i] <- cd-yr
}

#########################
# CORRELATIONS
#########################

corrs <- vector("numeric",51)

yields <- vector("numeric",1785)
weather <- vector("numeric",1785)
num <- 1

for (i in 1:51)
{  
  v <- JunePrecip[80:114,i]
  if (length(which(is.na(v))) > 0) {
    x <- detrended_cropdata[-which(is.na(v)),i]
    y <- v[-which(is.na(v))]
  }
  else
  {
    x <- detrended_cropdata[,i]
    y <- v
    yields[num:num+35] <- detrended_cropdata[,i]
    weather[num:num+35] <- v
    num <- num + 35
  }
  corrs[i] <- cor(x,y,use='complete')
}

hist(corrs,xlab='',ylab='',main='',col='blue',xlim=c(-1,1))

#########################
# GEOSTATISTICS
#########################

# Calculate Ranges

ranges <- vector("numeric",114)

for (i in 1:114)
{
  precip <- as.vector(DegreeDays[i,])
  if (length(which(is.na(precip))) > 0) {
    coo <- coords[-which(is.na(precip)),]
    precip <- precip[-which(is.na(precip))]    
  }
  else {coo <-coords}
  precipdf <- data.frame(precip)
  coordinates(precipdf) <- coo
  lzn.vgm = variogram(precip~1, precipdf)
  lzn.fit = fit.variogram(lzn.vgm, model = vgm(psill=max(lzn.vgm$gamma), "Sph", range=1.5,nugget=4000))
  plot(lzn.vgm, lzn.fit)
  if ((lzn.fit$range < 5) && (lzn.fit$range >= 0)) {ranges[i] <- lzn.fit$range}
}


#county <- shapefile(paste0(dir,"county/cb_2013_us_county_500k.shp"))
#sp_coords <- SpatialPoints(coords)
#crs(sp_coords) <- crs(county)

######################
# SAMPLE KRIGING
######################

year <- as.vector(DegreeDays[91,])
DD <- as.data.frame(year)
#spcoords <- SpatialPoints(coords)
DD[["x"]] <- coords[,1]
DD[["y"]] <- coords[,2]
DD[["elev"]] <- elevs
coordinates(DD) <- ~x+y

#iowa.grid <- spsample(DD, type = "regular")
#crs (iowa.grid) <- crs(iowa)
#crs (DD) <- crs(iowa)
#gridded(iowa.grid) <- TRUE

iowa.grid <- raster(paste0(dir,"gt30w100n90.tif"))
iowa.grid <- crop(iowa.grid, iowa)
plot(iowa.grid,legend=FALSE)
plot(iowa, add=T)

yeardf <- data.frame(year)
coordinates(yeardf) <- coords
lzn.vgm = variogram(year~1, yeardf)
lzn.fit = fit.variogram(lzn.vgm, model = vgm(psill=max(lzn.vgm$gamma), "Sph", range=1.5))
krigeDD <- krige(year~1,DD,iowa.grid,model = lzn.fit)

v.elev = variogram(year~elev, DD)
elev.model = fit.variogram(v.elev, vgm(psill=1500, "Sph", range=250))
krigeDD.elev <- krige(year~gt30w100n90,DD,iowa.grid,model = elev.model)
plot(v.elev,elev.model)

rv = list("sp.polygons", iowa, fill="blue")
spplot(krigeDD["var1.pred"], sp.layout=rv)
spplot(krigeDD["var1.var"], sp.layout=rv)

spplot(krigeDD.elev["var1.pred"], sp.layout=rv)
spplot(krigeDD.elev["var1.var"], sp.layout=rv)