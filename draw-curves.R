x <- 1:100
x <- x/20
 
y <- 0.4*(1.5*x/3-0.5*(x/3)^3)+0.6
y[61:100] <- 1

y2 <- 0.8*(1.5*x-0.5*(x)^3)+0.2
y2[21:100] <- 1

plot(x,y,xlim=c(0,5),ylim=c(0,1),col='white')
lines(x,y,xlim=c(0,5),ylim=c(0,1),lwd=3)
lines(x,y2,xlim=c(0,5),ylim=c(0,1),lwd=3)


