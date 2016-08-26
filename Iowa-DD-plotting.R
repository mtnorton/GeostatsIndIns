
# IOWA PLOTTING ANALYSIS

means1 <- matrix(NA, 51,2)
means2 <- matrix(NA, 51,2)

plot(rep(1,25),col='white',ylim=c(0,500),xlim=c(150,475),xlab="Elevation (m)",ylab="Degree Days Jun-Aug (Strike 25C)")


for (i in 1:51)
{
  points(rep(elevs[i],114),DegreeDays[,i]) #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
  means1[i,1:2] <- cbind(elevs[i],mean(DegreeDays[,i],na.rm=TRUE))
}

#-----------

plot(rep(1,25),col='white',ylim=c(0,500),xlim=c(40,41.5),xlab="Latitude",ylab="Degree Days Jun-Aug (Strike 25C)")

for (i in 1:51)
{
  points(rep(coords[i,2],114),DegreeDays[,i]) #,col=which(unique(sta_output[,1])==sta_output[i,1]),pch=which(unique(sta_output[,1])==sta_output[i,1]))
  means2[i,1:2] <- cbind(coords[i,2],mean(DegreeDays[,i],na.rm=TRUE))
}

par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(1,2,1,1))

plot(means1,xlab="",ylab="Mean CDD Jun-Aug (Strike 25C)",pch=19)
trendline <- summary(lm(means1[,2] ~ means1[,1]))$coefficients[1:2]
lines(c(0,abs(trendline[1]/trendline[2])),c(trendline[1],0),pch=22,lty=2)

plot(means2,xlab="",ylab="",pch=19,xlim=c(40,44))
trendline <- summary(lm(means2[,2] ~ means2[,1]))$coefficients[1:2]
lines(c(0,abs(trendline[1]/trendline[2])),c(trendline[1],0),pch=22,lty=2)

