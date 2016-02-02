library(sp)
library(rgdal)
library(geosphere)
library(FNN)

# First, read in all data

dir <- "/Users/mtnorton/Dropbox/temp/Distance measures/"

allstations <- read.csv(paste0(dir,"all-station-coords.csv"))
AS <- SpatialPoints(cbind(cbind(allstations$LONG,allstations$LAT)))
crs(AS) <- CRS("+init=epsg:4326")
AS_LL <- spTransform(AS, CRS("+init=epsg:3116"))

govt <- read.csv(paste0(dir,"coffeestations.csv"))
GS <- SpatialPoints(cbind(cbind(govt$LONG,govt$LAT)))
crs(GS) <- CRS("+init=epsg:4326")
GS_LL <- spTransform(GS, CRS("+init=epsg:3116"))

# Split census data by projection, reproject western edge

year <- c("06","06","07","07","08","08","09","09","10","10")
month <- rep(c("01","02"),5)

for (i in 1: 10)
{
  filename <- paste0("/Users/mtnorton/Dropbox/temp/Production_Records/prod",year[i],"_",month[i],".csv")
  prod <- read.csv(filename)
  prod1 <- prod[which(prod[,5]==1),]
  prod2 <- prod[which(prod[,5]==2),]
  
  prod1df <- data.frame(prod1[,1],prod1[,4],prod1[,5])
  names(prod1df) <- c("Y_LOTE","CCOD_ECOTO","proj")
  prod2df <- data.frame(prod2[,1],prod2[,4],prod2[,5])
  names(prod2df) <- c("Y_LOTE","CCOD_ECOTO","proj")
  
  sp_prod1 <- SpatialPointsDataFrame(cbind(prod1$CENT_X, prod1$CENT_Y), prod1df, proj4string=CRS("+init=epsg:3116"))
  #crs(sp_prod1) <- CRS("+init=epsg:3116")
  
  sp_prod2_w <- SpatialPointsDataFrame(cbind(prod2$CENT_X, prod2$CENT_Y), prod2df, proj4string=CRS("+init=epsg:3115"))
  #crs(sp_prod2_w) <- CRS("+init=epsg:3115")
  sp_prod2 <- spTransform(sp_prod2_w, CRS("+init=epsg:3116"))
  
  sp_prod <- rbind(sp_prod1,sp_prod2)
  
  # SR4_LL <- spTransform(SR4, CRS("+init=epsg:4326"))
  
  AS_coords <- coordinates(AS_LL)
  GS_coords <- coordinates(GS_LL)
  prod_coords <- coordinates(sp_prod)
  
  # Now output production maps
  
  prod_for_map <- matrix(NA, nrow(prod), 5)
  prod_for_map[,1:2] <- prod_coords
  prod_for_map[,3] <- sp_prod$Y_LOTE
  prod_for_map[,4] <- as.character(sp_prod$CCOD_ECOTO)
  prod_for_map[,5] <- sp_prod$proj
  colnames(prod_for_map) <- c("CENT_X","CENT_Y","Y_LOTE","CCOD_ECOTO","proj")
  
  filename <- paste0("/Users/mtnorton/Dropbox/temp/Production_Records/prodmap",year[i],month[i],".csv")
  
  write.csv (prod_for_map, filename)
}
# Now calculate distances to weather stations

nnAS = get.knnx(AS_coords,prod_coords,k=1)
nnGS = get.knnx(GS_coords,prod_coords,k=1)

quantile(nnAS$nn.dist/1000)
quantile(nnGS$nn.dist/1000)

hist(nnAS$nn.dist/1000,main="Distance to Nearest Weather Station (All Stations)",xlab="Distance (km)", ylab="# of Lote")
hist(nnGS$nn.dist/1000,main="Distance to Nearest Weather Station (Gov't Stations)",xlab="Distance (km)", ylab="# of Lote")



