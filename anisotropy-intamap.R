dataobs <- SpatialPointsDataFrame(stations,as.data.frame(DegreeDays[92,]),proj4string="+proj=utm +zone=15 ellps=WGS84")

# or using createIntamapObject
obj = createIntamapObject(
  observations = dataobs
  #predictionLocations = 100
  #targetCRS = "+proj=utm +zone=15 ellps=WGS84" #,class = c("idw")
)

estimateAnisotropy(obj)