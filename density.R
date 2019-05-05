args<-commandArgs(TRUE)
fragment <- scan(args[1])
breaks <- seq(0,410,by=10)
fragment[fragment>400]<-410
png(paste(args[2],'.insertSize.png',sep=''),type='cairo-png')
plot(density(fragment,bw=2),xlab="", main = paste(args[2],'insert size distribution',sep=' '),lwd=2,col=2)
dev.off()

