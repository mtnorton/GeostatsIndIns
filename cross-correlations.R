# Find things at a certain distance, 8 steps

year <- 1:114
# Four separate correlations for different indexes
# For plots - I manually change variables in the loop but there is a more elegant 
#     way to do this.
corrs1 <- matrix(NA,length(year)*8,2)
corrs2 <- matrix(NA,length(year)*8,2)
corrs3 <- matrix(NA,length(year)*8,2)
corrs4 <- matrix(NA,length(year)*8,2)

for (k in 1:length(year))
{
  short_dist <- 0
  long_dist <- 50000
  for (j in 1:8)
  {
    dist100 <- which((distances<long_dist & distances>short_dist))
    
    dist100values <- distances[dist100]
    dist100i <- dist100%%51
    dist100j <- floor(dist100/51)+1
    
    scatt_values <- matrix(NA, length(dist100), 2)
    
    for (i in 1:length(dist100))
    {
      scatt_values[i,1] <- DegreeDays[year[k],dist100i[i]]
      scatt_values[i,2] <- DegreeDays[year[k],dist100j[i]]
    }
    
    plot(scatt_values)
    corrs4[((k-1)*8)+j,1] <- cor(scatt_values[,1], scatt_values[,2],use="na.or.complete")
    corrs4[((k-1)*8)+j,2] <- j
  
    short_dist <- short_dist + 50000
    long_dist <- long_dist + 50000
  }
  print(k)
}

# corrs1 - precip total June-August
# corrs2 - precip total June only
# corrs3 - GDD strike 30 June-August
# corrs4 - GDD strike 30 July only


par(mfrow=c(2,2),mar=c(2,4,1.5,1))

axis_labels <- c('<50km','100km','150km','200km','250km','300km','350km','400km')

corrs.df <- as.data.frame(corrs1)
boxplot(V1~V2,corrs.df,ylab='Correlation',xaxt='n',xlab='Distance')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June-Aug. Precip. Totals')

corrs.df <- as.data.frame(corrs2)
boxplot(V1~V2,corrs.df,ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June Precip. Totals')

corrs.df <- as.data.frame(corrs3)
boxplot(V1~V2,corrs.df,ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June-Aug. CDD (30 C)')

corrs.df <- as.data.frame(corrs4)
boxplot(V1~V2,corrs.df,ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June CDD (30 C)')


#-----------------------------
#--- OLD PLOTTING METHOD
#-----------------------------
plot(c(1:8),corrs1[,1], col='white',ylim=c(-0.2,1), ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June-Aug Precipitation Totals')
for (i in 1:length(year))
{
  lines(corrs1[,i])
}
for (i in 1:8)
{
  points(i,mean(corrs1[i,]),col='black',pch=16)
  #if (i>1){lines(c(i-1,i),c(mean(corrs1[i-1,]),mean(corrs1[i,])),col='white',lwd=2)}
}
lines(c(1,8),c(0,0),col='white')

#-----------------------------
plot(c(1:8),corrs3[,1], col='white',ylim=c(-0.2,1), ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June-August CDD (29 C)')
for (i in 1:length(year))
{
  lines(corrs3[,i])
}
for (i in 1:8)
{
  points(i,mean(corrs3[i,]),col='white',pch=16)
  if (i>1){lines(c(i-1,i),c(mean(corrs3[i-1,]),mean(corrs3[i,])),col='white',lwd=2)}
}
lines(c(1,8),c(0,0),col='white')

#-----------------------------
plot(c(1:8),corrs2[,1], col='white',ylim=c(-0.2,1), ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='June-August Rainy Days')
for (i in 1:length(year))
{
  lines(corrs2[,i])
}
for (i in 1:8)
{
  points(i,mean(corrs2[i,]),col='white',pch=16)
  if (i>1){lines(c(i-1,i),c(mean(corrs2[i-1,]),mean(corrs2[i,])),col='white',lwd=2)}
}
lines(c(1,8),c(0,0),col='white')

#-----------------------------
plot(c(1:8),corrs4[,1], col='white',ylim=c(-0.2,1), ylab='Correlation',xaxt='n',xlab='')
axis(side = 1, at = 1:8, labels = axis_labels, tick = 1)
title(main='May CDD (29 C)')
for (i in 1:length(year))
{
  lines(corrs4[,i])
}
for (i in 1:8)
{
  points(i,mean(corrs4[i,],na.rm=TRUE),col='white',pch=16)
  if (i>1){lines(c(i-1,i),c(mean(corrs4[i-1,],na.rm=TRUE),mean(corrs4[i,],na.rm=TRUE)),col='white',lwd=2)}
}
lines(c(1,8),c(0,0),col='white')

# Now export for Excel

write.csv(corrs,"/Users/mtnorton/GeostatsIndIns/plots/corrs.csv")
