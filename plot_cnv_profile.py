#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os 
import sys
import argparse
from argparse import RawTextHelpFormatter

parser = argparse.ArgumentParser(description="Pyclone modules.",formatter_class=RawTextHelpFormatter)
parser.add_argument('-c','--config',help="A list file contains cnv ratio and BAF files, one sample per line, seperated by tab.\n   #sampleID\tratio\tBAF\n",required=True)
parser.add_argument('-s','--step', help="Step for slimming of input data, default 20. 0 or 1 for non-slimming", default=20, type=int)
parser.add_argument('-g','--groupname', help="Group name for the list", required=True)
parser.add_argument('-r','--genome', help="Ref genome", default="b37")
parser.add_argument('-o','--outdir',help="Output directory",required=True)
argv=parser.parse_args()

def safe_open(file_name,mode='r'):
	try:
		if not file_name.endswith('.gz'):
			return open(file_name,mode)
		else:
			import gzip
			return gzip.open(file_name,mode)
	except IOError:
		print file_name + ' do not exist!'

def slim_data(infile, outfile, step=20):
	if not os.path.exists(infile):
		infile += '.gz'
	out = open(outfile,'w')
	i = 0
	for line in safe_open(infile):
		if i == 0:
			i += 1
			out.write(line)
			continue
		if i % step == 1:
			out.write(line)
	out.close()


outdir = argv.outdir
if not os.path.exists(outdir):
	assert not os.system('mkdir '+outdir)
rm_files = []
rm_files.append(os.path.join(outdir, 'slim.config'))

slim_input = open(os.path.join(outdir, 'slim.config'),"w")
for line in open(argv.config):
	if line.startswith('#'):
		continue
	array = line.strip().split()
	if abs(argv.step) <= 1:
		slim_input.write(line)
	else:
		ratio_slim = os.path.join(outdir,os.path.basename(array[1])+'.slim')
		baf_slim = os.path.join(outdir,os.path.basename(array[2])+'.slim')
		rm_files.append(ratio_slim)
		rm_files.append(baf_slim)
		slim_data(array[1], ratio_slim, step=abs(argv.step))
		slim_data(array[2], baf_slim, step=abs(argv.step))
		slim_input.write('%s\t%s\t%s\n' % (array[0],ratio_slim,baf_slim))
slim_input.close()

chrLen_genome = "249250621,243199373,198022430,191154276,180915260,171115067,159138663,146364022,141213431,135534747,135006516,133851895,115169878,107349540,102531392,90354753,81195210,78077248,59128983,63025520,48129895,51304566,155270560,59373566"
centromere_genome = "125000000,93300000,91000000,50400000,48400000,61000000,59900000,45600000,49000000,40200000,53700000,35800000,17900000,17600000,19000000,36600000,24000000,17200000,26500000,27500000,13200000,14700000,60600000,12500000"
if "38" in argv.genome:
	chrLen_genome = "248956422,242193529,198295559,190214555,181538259,170805979,159345973,145138636,138394717,133797422,135086622,133275309,114364328,107043718,101991189,90338345,83257441,80373285,58617616,64444167,46709983,50818468,156040895,57227415"
	centromere_genome = "123400000,93900000,90900000,50000000,48800000,59800000,60100000,45200000,43000000,39800000,53400000,35500000,17700000,17200000,19000000,36800000,25100000,18500000,26200000,28100000,12000000,15000000,61000000,10400000"

rscript = '''
setwd("%s")
ploidy <- 2
### chromosome length, centromere position
chrLen<-c(%s)
names(chrLen)<-c(1:22,'X','Y')
centromere=c(%s)
names(centromere)<-c(1:22,'X','Y')
cumChrLen<-c(0,cumsum(chrLen))[-length(chrLen)]#??Ⱦɫ?????ۼƳ??ȣ???ɾȥ1-22-X???ۼƳ???
names(cumChrLen)<-c(1:22,'X','Y')
cumCen<-cumChrLen+centromere
names(cumCen)<-c(1:22,'X','Y')

files <-read.table("%s",sep="\\t")
png(filename = paste("%s",".CNV_profile.png",sep = ""),width=600*2,height=nrow(files)*120*2,type="cairo-png",res=72*2)
par(mfrow=c(nrow(files)*2+2,1),mar=c(0.3,2,0.3,5))

plot(1:10,rep(0,10),col='white',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',ylim=c(0,6))
text(5,2,"%s",cex=1.5)

#par(mfrow=c(2,1),mar=c(0.3,2,0.3,5))

for(j in 1:nrow(files)){
	### CNV profile
	par(mar=c(0,2,0.3,5))
	cat("Reading CNV file ...\\n")
	ratio <-read.table(as.character(files[j,2]), head=TRUE,sep="\\t")
	maxLevelToPlot <- 3

	for (i in c(1:length(ratio$Ratio))) {#ratio$Ratio????ȡ?????ݿ?ratio?е?Ratio?У?length(ratio$Ratio)?൱??Ratio?е?????
		if (ratio$Ratio[i]>maxLevelToPlot) {
			ratio$Ratio[i]=maxLevelToPlot;
		}
	}
	ratio2<-subset(ratio,Chromosome %%in%% 1:22)
	ratio2$x<-cumChrLen[as.character(ratio2$Chromosome)]+ratio2$Start#??ratio2???ݿ???????һ??x????24??Ⱦɫ?廭??ͬһ??X???ϣ?x??ÿ??CNV??????????X???ϵ???ʼλ??
	ratio2$color<-"darkolivegreen3"#??ratio2???ݿ???????һ??color??ȫ??????Ϊdarkolivegreen3
	ratio2$color[ratio2$CopyNumber>2]<-"firebrick3"#ratio2???ݿ???color?У?copynumber????2???ú?ɫ
	ratio2$color[ratio2$CopyNumber<2]<-"mediumblue"#ratio2???ݿ???color?У?copynumberС??2????��ɫ

	plot(ratio2$x,ratio2$Ratio*2,xlim = c(0,cumChrLen[23]),ylim = c(0,maxLevelToPlot*ploidy),xlab = "",ylab = "",pch = ".",col = ratio2$color,bty='n',xaxt='n',yaxt='n')
	points(ratio2$x,ratio2$CopyNumber,cex=1.2,pch = ".",col = colors()[24])
	#plot(ratio2$x,ratio2$Ratio*2,ylim = c(0,maxLevelToPlot*ploidy),xlab = "",ylab = "",pch = ".",col = ratio2$color,bty=']',xaxt='n',yaxt='n')
#	axis(side=2,line=-0.95,at=0:6,labels=0:6,col='darkgrey',xpd=NA)
	segments(0,-0.1,0,6.1,lwd=1,xpd=T)
	for (i in 2:23){
		segments(cumChrLen[i],0.5,cumChrLen[i],5.5,lwd=1,lty=22,col="darkgrey")
	}
	for (i in 0:6){
		segments(-20000000,i,0,i,xpd=T)
		text(-10000000,i,i,pos=2,adj=0,xpd=T,cex=0.8,lwd=0.8)
	}
	## centromere
	text(cumCen[1:22],rep(2,22),labels=rep(".",22),cex=2,adj=0)#??????˿��??λ?ã??õ???ʾ?????Ĵ?С????ͨ????2??
	#text(cumCen[1:22],rep(2,22),labels=rep(".",22),cex=2)#??ָ??adjʱ????˿��??λ?????ơ?
	text(cumCen[1:22],rep(2.05,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(2.03,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(1.98,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(1.95,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	mtext(side=2,line=0,"Copy Number",cex=0.5)
	### BAF
	BAF<-read.table(as.character(files[j,3]),head=TRUE,sep="\\t")
	BAF2<-subset(BAF,Chromosome %%in%% 1:22)
	BAF2$x<-cumChrLen[as.character(BAF2$Chromosome)]+BAF2$Position
	BAF2$color<-colors()[1]
	BAF2$color[BAF2$A==0.5]<-colors()[92]
	BAF2$color[BAF2$A!=0.5 & BAF2$A>=0]<-colors()[62]

	par(mar=c(0.3,2,0,5))
	plot(BAF2$x,BAF2$BAF,ylim = c(-0.1,1.1),xlab = "",ylab = "",pch = ".",col = BAF2$color,bty='n',xaxt='n',yaxt='n')
	for (i in 1:22){
     	  lBAF<-subset(BAF2,Chromosome==i)
        tt<-1
     	  pres<-1
        if (length(lBAF$FittedA)>4){
     	          for (n in c(2:(length(lBAF$FittedA)-pres-1))) {
           	            if (lBAF$FittedA[n]==lBAF$FittedA[n+pres]) {
                 	              tt[length(tt)+1] <- n
                        }
                }
                points(lBAF$x[tt],lBAF$FittedA[tt],pch = ".",col = colors()[24],cex=1.2)
                points(lBAF$x[tt],lBAF$FittedB[tt],pch = ".",col = colors()[24],cex=1.2)
        }
	}
	segments(0,-0.02,0,1.02,lwd=1,xpd=T)
	for (i in 2:23){
		segments(cumChrLen[i],0,cumChrLen[i],1,lwd=1,lty=22,col="darkgrey")
	}
	for (i in seq(0,1,by=0.2)){
		segments(-20000000,i,0,i,xpd=T,lwd=0.8)
		text(-10000000,i,i,pos=2,adj=0,xpd=T,cex=0.8)
	}
	## sample label
	if(nrow(files)>1){
		text(cumChrLen[23]*1,0.95,as.character(files[j,1]),xpd=T,cex=1.0,pos=4,adj=0)
	}
	mtext(side=2,line=0,"BAF",cex=0.5)
}

plot(cumChrLen[1:23], rep(0,23), xlim = c(0,cumChrLen[23]),ylim = c(0,maxLevelToPlot*ploidy), col='white',bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
text(cumCen[1:22],5.7+c(rep(0,12),rep(c(-0.1,-0.3),5)),labels=1:22,xpd=T,cex=0.8)
#cumChrLen
dev.off()
## pdf plot
pdf(paste("%s",".CNV_profile.pdf",sep = ""),width=600*7/480,height=nrow(files)*120*7/480)
par(mfrow=c(nrow(files)*2+2,1),mar=c(0.3,2,0.3,5))

plot(1:10,rep(0,10),col='white',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',ylim=c(0,6))
text(5,2,"%s",cex=1.5)

#par(mfrow=c(2,1),mar=c(0.3,2,0.3,5))

for(j in 1:nrow(files)){
	### CNV profile
	par(mar=c(0,2,0.3,5))
	cat("Reading CNV file ...\\n")
	ratio <-read.table(as.character(files[j,2]), head=TRUE,sep="\\t")
	maxLevelToPlot <- 3

	for (i in c(1:length(ratio$Ratio))) {#ratio$Ratio????ȡ?????ݿ?ratio?е?Ratio?У?length(ratio$Ratio)?൱??Ratio?е?????
		if (ratio$Ratio[i]>maxLevelToPlot) {
			ratio$Ratio[i]=maxLevelToPlot;
		}
	}
	ratio2<-subset(ratio,Chromosome %%in%% 1:22)
	ratio2$x<-cumChrLen[as.character(ratio2$Chromosome)]+ratio2$Start#??ratio2???ݿ???????һ??x????24??Ⱦɫ?廭??ͬһ??X???ϣ?x??ÿ??CNV??????????X???ϵ???ʼλ??
	ratio2$color<-"darkolivegreen3"#??ratio2???ݿ???????һ??color??ȫ??????Ϊdarkolivegreen3
	ratio2$color[ratio2$CopyNumber>2]<-"firebrick3"#ratio2???ݿ???color?У?copynumber????2???ú?ɫ
	ratio2$color[ratio2$CopyNumber<2]<-"mediumblue"#ratio2???ݿ???color?У?copynumberС??2????��ɫ

	plot(ratio2$x,ratio2$Ratio*2,xlim = c(0,cumChrLen[23]),ylim = c(0,maxLevelToPlot*ploidy),xlab = "",ylab = "",pch = ".",col = ratio2$color,bty='n',xaxt='n',yaxt='n')
	points(ratio2$x,ratio2$CopyNumber,cex=1.2,pch = ".",col = colors()[24])
	#plot(ratio2$x,ratio2$Ratio*2,ylim = c(0,maxLevelToPlot*ploidy),xlab = "",ylab = "",pch = ".",col = ratio2$color,bty=']',xaxt='n',yaxt='n')
#	axis(side=2,line=-0.95,at=0:6,labels=0:6,col='darkgrey',xpd=NA)
	segments(0,-0.1,0,6.1,lwd=1,xpd=T)
	for (i in 2:23){
		segments(cumChrLen[i],0.5,cumChrLen[i],5.5,lwd=1,lty=22,col="darkgrey")
	}
	for (i in 0:6){
		segments(-20000000,i,0,i,xpd=T)
		text(-10000000,i,i,pos=2,adj=0,xpd=T,cex=0.8,lwd=0.8)
	}
	## centromere
	text(cumCen[1:22],rep(2,22),labels=rep(".",22),cex=2,adj=0)#??????˿��??λ?ã??õ???ʾ?????Ĵ?С????ͨ????2??
	#text(cumCen[1:22],rep(2,22),labels=rep(".",22),cex=2)#??ָ??adjʱ????˿��??λ?????ơ?
	text(cumCen[1:22],rep(2.05,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(2.03,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(1.98,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	text(cumCen[1:22],rep(1.95,22),labels=rep(".",22),cex=2,adj=0)#ͻ????˿��??λ??
	mtext(side=2,line=0,"Copy Number",cex=0.5)
	### BAF
	BAF<-read.table(as.character(files[j,3]),head=TRUE,sep="\\t")
	BAF2<-subset(BAF,Chromosome %%in%% 1:22)
	BAF2$x<-cumChrLen[as.character(BAF2$Chromosome)]+BAF2$Position
	BAF2$color<-colors()[1]
	BAF2$color[BAF2$A==0.5]<-colors()[92]
	BAF2$color[BAF2$A!=0.5 & BAF2$A>=0]<-colors()[62]

	par(mar=c(0.3,2,0,5))
	plot(BAF2$x,BAF2$BAF,ylim = c(-0.1,1.1),xlab = "",ylab = "",pch = ".",col = BAF2$color,bty='n',xaxt='n',yaxt='n')
	for (i in 1:22){
     	  lBAF<-subset(BAF2,Chromosome==i)
        tt<-1
     	  pres<-1
        if (length(lBAF$FittedA)>4){
     	          for (n in c(2:(length(lBAF$FittedA)-pres-1))) {
           	            if (lBAF$FittedA[n]==lBAF$FittedA[n+pres]) {
                 	              tt[length(tt)+1] <- n
                        }
                }
                points(lBAF$x[tt],lBAF$FittedA[tt],pch = ".",col = colors()[24],cex=1.2)
                points(lBAF$x[tt],lBAF$FittedB[tt],pch = ".",col = colors()[24],cex=1.2)
        }
	}
	segments(0,-0.02,0,1.02,lwd=1,xpd=T)
	for (i in 2:23){
		segments(cumChrLen[i],0,cumChrLen[i],1,lwd=1,lty=22,col="darkgrey")
	}
	for (i in seq(0,1,by=0.2)){
		segments(-20000000,i,0,i,xpd=T,lwd=0.8)
		text(-10000000,i,i,pos=2,adj=0,xpd=T,cex=0.8)
	}
	## sample label
	if(nrow(files)>1){
		text(cumChrLen[23]*1,0.95,as.character(files[j,1]),xpd=T,cex=1.0,pos=4,adj=0)
	}
	mtext(side=2,line=0,"BAF",cex=0.5)
}

plot(cumChrLen[1:23], rep(0,23), xlim = c(0,cumChrLen[23]),ylim = c(0,maxLevelToPlot*ploidy), col='white',bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
text(cumCen[1:22],5.7+c(rep(0,12),rep(c(-0.1,-0.3),5)),labels=1:22,xpd=T,cex=0.8)
#cumChrLen
dev.off()
''' % (outdir,chrLen_genome,centromere_genome,os.path.join(outdir, 'slim.config'), argv.groupname, argv.groupname, argv.groupname, argv.groupname)
open(os.path.join(outdir,'cnv_profile.R'),'w').write(rscript)
assert not os.system('Rscript %s' % (os.path.join(outdir,'cnv_profile.R')))
assert not os.system('rm -rf %s'%(' '.join(rm_files)))
