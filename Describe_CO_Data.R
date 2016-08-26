# Code for describing data

library(sp)
library(raster)

# Get area of ecotopos

ecotopos <- shapefile("/Users/mtnorton/Dropbox/Coffee Insurance/Data/coffeemission_data/ecotopos/POLIGONOS_ECOTOPOS_CAFETEROS.shp")

ecotopos$Region <- 0

ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="1"),which(substr(ecotopos$Ecotopo,4,4)=="A"))] <- 1
ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="1"),which(substr(ecotopos$Ecotopo,4,4)=="B"))] <- 2
ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="2"),which(substr(ecotopos$Ecotopo,4,4)=="A"))] <- 3
ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="2"),which(substr(ecotopos$Ecotopo,4,4)=="B"))] <- 4
ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="3"),which(substr(ecotopos$Ecotopo,4,4)=="A"))] <- 5
ecotopos$Region[intersect(which(substr(ecotopos$Ecotopo,1,1)=="3"),which(substr(ecotopos$Ecotopo,4,4)=="B"))] <- 6
ecotopos$Region[which(substr(ecotopos$Ecotopo,1,1)=="4")] <- 7

sum(ecotopos$Area_Ha[which(ecotopos$Region==1)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==2)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==3)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==4)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==5)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==6)])
sum(ecotopos$Area_Ha[which(ecotopos$Region==7)])

sum(ecotopos$Area_Ha)

nrow(ecotopos[which(ecotopos$Region==1),])
nrow(ecotopos[which(ecotopos$Region==2),])
nrow(ecotopos[which(ecotopos$Region==3),])
nrow(ecotopos[which(ecotopos$Region==4),])
nrow(ecotopos[which(ecotopos$Region==5),])
nrow(ecotopos[which(ecotopos$Region==6),])
nrow(ecotopos[which(ecotopos$Region==7),])

# Match station coordinates with ecotopo and then regions
# Should run HeatIndex.R

crs(S) <- CRS("+proj=longlat +datum=WGS84")
S <- spTransform(S,crs(ecotopos))
regions <- over(S,ecotopos)$Region
length(which(regions==1))
length(which(regions==2))
length(which(regions==3))
length(which(regions==4))
length(which(regions==5))
length(which(regions==6))
length(which(regions==7))
length(which(is.na(regions)))

# Now use HeatIndexCenicafe.R

SCC <- cbind(stations_cc[,10],stations_cc[,9])
SCC <- SpatialPoints(SCC)
crs(SCC) <- CRS("+proj=longlat +datum=WGS84")
SCC <- spTransform(SCC,crs(ecotopos))
regions <- over(SCC,ecotopos)$Region
length(which(regions==1))
length(which(regions==2))
length(which(regions==3))
length(which(regions==4))
length(which(regions==5))
length(which(regions==6))
length(which(regions==7))
length(which(is.na(regions)))

################
# DESCRIBE Y_LOTE
################

y_lote <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/Base_Pron.csv")

par(mfrow=c(4,2),mar=c(4,4,2,2))

hist(y_lote$log_y_lote[which(y_lote$region=="1A")],main="Region 1A",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="1B")],main="Region 1B",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="2A")],main="Region 2A",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="2B")],main="Region 2B",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="3A")],main="Region 3A",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="3B")],main="Region 3B",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote[which(y_lote$region=="4")],main="Region 4",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))
hist(y_lote$log_y_lote,main="All Regions",col="grey",xlab="Log(yield)",ylab="# Lote",xlim=c(-8,8))

