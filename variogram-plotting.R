sig2 <- 1
range <- 1

models <- matrix(NA, 200, 6)
models[,1] <- (1:200)/100

#Spherical
models[,2] <- (1.5*models[,1]-0.5*(models[,1])^3)

#Exponential
models[,3] <- (1-exp(-3*models[,1]))

#Gaussian
models[,4] <- (1-exp(-3*models[,1]^2))
  
#Power 
models[,5] <- models[,1]^0.4
#models[,6] <- (models[,1]+1)^0.4 # because no sill

### PLOT
par(mfrow=c(2,2))

plot(models[,1:2],col="white",xaxt="n",yaxt="n", main="Spherical",xlim=c(0,2),ylim=c(0,1.1))
lines(models[1:100,1:2])
lines(c(1,2),c(1,1))
plot(models[,c(1,3)],col="white",xaxt="n",yaxt="n", main="Exponential",xlim=c(0,2),ylim=c(0,1.1))
lines(models[,c(1,3)])
plot(models[,c(1,4)],col="white",xaxt="n",yaxt="n", main="Gaussian",xlim=c(0,2),ylim=c(0,1.1))
lines(models[,c(1,4)])
plot(models[,c(1,5)],col="white",xaxt="n",yaxt="n", main="Power (0.4)",xlim=c(0,2),ylim=c(0,1.1))
lines(models[,c(1,5)])
#lines(models[,1]+1,models[,6])
