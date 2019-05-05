args<-commandArgs(TRUE)
fragment <- scan(args[1])
breaks <- seq(0,410,by=10)
fragment[fragment>400]<-410
png(paste(args[2],'.insertSize.png',sep=''),type='cairo-png')
hist(fragment, main = paste(args[2],'insert size distribution',sep=' '),breaks=breaks)
dev.off()

