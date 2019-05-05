library("VennDiagram")
args<-commandArgs(TRUE)
#####now this code is specfic for noRef_zhuanluzu
vennlist<-args[1]####gene table;SAMPLE1vsSAMPLE2.DEGlist.txt,SAMPLE2vsSAMPLE3.DEGlist.txt
namelist<-args[2]####compare name list, same order with vennlist;#SAMPLE1vsSAMPLE2,SAMPLE2vsSAMPLE3
outdir<-args[3] ######outdir

Outdir<-paste(outdir,"/",sep="")
dir.create(Outdir)
array<-as.vector(unlist(strsplit(namelist,",")))
vennlist_array<-as.vector(unlist(strsplit(vennlist,",")))

mylist=list()
for (j in 1:length(array)) {
	a<-read.table(vennlist_array[j])
	mylist[[array[j]]]<-as.matrix(a)
}
comparename<-gsub(",","_",namelist)
ven=mylist
array1<-as.data.frame(array)
array1$name1<-""
array1$name2<-""
for (h in 1:dim(array1)[1]) {
	array1$name1[h]<-unlist(strsplit(as.character(array1$array[h]),"vs"))[1]
	array1$name2[h]<-unlist(strsplit(as.character(array1$array[h]),"vs"))[2]
}	
###################	
#####venn plot#####
###################
if(length(array)==2){
    	if(length(ven[[1]]) > length(ven[[2]])) cat.pos <- c(-15,15) else cat.pos <- c(15,-15)
	v=venn.diagram(ven,alpha=0.4,filename=NULL,cex=1.4,cat.cex=1.4,margin=0.05,scaled=F,fill=c("Gold1", "darkorchid1"),cat.pos=cat.pos,cat.dist=rep(0.05,2),lty=0,fontfamily="sans", cat.fontfamily="sans")
    	grid.newpage()
    	pdf(paste(Outdir,comparename,".DEG_Venn_diagram.pdf",sep=""))
    	grid.draw(v)
    	dev.off()
		png(paste(Outdir,comparename,".DEG_Venn_diagram.png",sep=""),type="cairo-png")
        grid.draw(v)
        dev.off()
	genelist=unique(c(ven[[1]],ven[[2]]))
	yes_yes<-intersect(ven[[1]],ven[[2]])
	yes_no<-ven[[1]][!(ven[[1]] %in% ven[[2]])]
	no_yes<-ven[[2]][!(ven[[2]] %in% ven[[1]])]
	write.table(as.data.frame(genelist),paste(Outdir,"all.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no),paste(Outdir,array[1],"-yes-",array[2],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes),paste(Outdir,array[1],"-no-",array[2],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
}

if(length(array)==3){
        v=venn.diagram(ven,filename=NULL,cex=1.4,cat.cex=1,margin=0.05,scaled=F,fill=c("Gold1","Cyan","MediumPurple"),cat.pos=c(-15,15,180),cat.dist=rep(0.05,3),lty=0,fontfamily="sans", cat.fontfamily="sans",alpha=0.4)
        grid.newpage()
        pdf(paste(Outdir,comparename,".DEG_Venn_diagram.pdf",sep=""))
        grid.draw(v)
        dev.off()
        png(paste(Outdir,comparename,".DEG_Venn_diagram.png",sep=""),type="cairo-png")
        grid.draw(v)
        dev.off()
	genelist=unique(c(ven[[1]],ven[[2]],ven[[3]]))
	yes_yes_yes<-intersect(ven[[1]],ven[[2]])[intersect(ven[[1]],ven[[2]]) %in%  ven[[3]]]
	yes_yes_no<-intersect(ven[[1]],ven[[2]])[!(intersect(ven[[1]],ven[[2]]) %in%  ven[[3]] )]
	yes_no_yes<-intersect(ven[[1]],ven[[3]])[!(intersect(ven[[1]],ven[[3]]) %in%  ven[[2]])]
	yes_no_no<-ven[[1]][!(ven[[1]] %in% c(ven[[2]],ven[[3]]))]
	no_yes_yes<-intersect(ven[[2]],ven[[3]])[!(intersect(ven[[2]],ven[[3]]) %in% ven[[1]])]
	no_yes_no<-ven[[2]][!(ven[[2]] %in% c(ven[[1]],ven[[3]]))]
	no_no_yes<-ven[[3]][!(ven[[3]] %in% c(ven[[1]],ven[[2]]))]
	write.table(as.data.frame(genelist),paste(Outdir,"all.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
}

if(length(array)==4){
        v=venn.diagram(ven, cex=1.2,cat.cex=1,scaled=F,filename=NULL, fontfamily="sans", cat.fontfamily="sans",  fill=c("cornflowerblue", "green", "Gold1", "darkorchid1"),  margin=0.1,cat.dist=c(0.25,0.25,0.12,0.12),lty=0,alpha=0.4)
        grid.newpage()
        pdf(paste(Outdir,comparename,".DEG_Venn_diagram.pdf",sep=""))
        grid.draw(v)
        dev.off()
        png(paste(Outdir,comparename,".DEG_Venn_diagram.png",sep=""),type="cairo-png")
        grid.draw(v)
        dev.off()
	genelist=unique(c(ven[[1]],ven[[2]],ven[[3]],ven[[4]]))
	yes_yes_yes_yes<-intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[4]]))
	yes_yes_yes_no<-intersect(intersect(ven[[1]],ven[[2]]),ven[[3]])[!(intersect(intersect(ven[[1]],ven[[2]]),ven[[3]]) %in% ven[[4]])]
	yes_yes_no_yes<-intersect(intersect(ven[[1]],ven[[2]]),ven[[4]])[!(intersect(intersect(ven[[1]],ven[[2]]),ven[[4]]) %in% ven[[3]])]
	yes_no_yes_yes<-intersect(intersect(ven[[1]],ven[[3]]),ven[[4]])[!(intersect(intersect(ven[[1]],ven[[3]]),ven[[4]]) %in% ven[[2]])]
	no_yes_yes_yes<-intersect(intersect(ven[[2]],ven[[3]]),ven[[4]])[!(intersect(intersect(ven[[2]],ven[[3]]),ven[[4]]) %in% ven[[1]])]
	yes_yes_no_no<-intersect(ven[[1]],ven[[2]])[!( intersect(ven[[1]],ven[[2]]) %in% c(ven[[3]],ven[[4]]))]
	yes_no_yes_no<-intersect(ven[[1]],ven[[3]])[!( intersect(ven[[1]],ven[[3]]) %in% c(ven[[2]],ven[[4]]))]
	no_yes_yes_no<-intersect(ven[[2]],ven[[3]])[!( intersect(ven[[2]],ven[[3]]) %in% c(ven[[1]],ven[[4]]))]
	yes_no_no_yes<-intersect(ven[[1]],ven[[4]])[!( intersect(ven[[1]],ven[[4]]) %in% c(ven[[2]],ven[[3]]))]
	no_yes_no_yes<-intersect(ven[[2]],ven[[4]])[!( intersect(ven[[2]],ven[[4]]) %in% c(ven[[1]],ven[[3]]))]
	no_no_yes_yes<-intersect(ven[[3]],ven[[4]])[!( intersect(ven[[3]],ven[[4]]) %in% c(ven[[1]],ven[[2]]))]
	yes_no_no_no<-ven[[1]][! (ven[[1]] %in% c(ven[[2]],ven[[3]],ven[[4]]))]
	no_yes_no_no<-ven[[2]][! (ven[[2]] %in% c(ven[[1]],ven[[3]],ven[[4]]))]
	no_no_yes_no<-ven[[3]][! (ven[[3]] %in% c(ven[[1]],ven[[2]],ven[[4]]))]
	no_no_no_yes<-ven[[4]][! (ven[[4]] %in% c(ven[[1]],ven[[2]],ven[[3]]))]
	write.table(as.data.frame(genelist),paste(Outdir,"all.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_yes_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_yes_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_no_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_yes_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_yes_no_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_no_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_yes_yes_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_no_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_yes_no_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_no_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_no_no_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_yes_no_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(no_no_yes_no),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_no_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-no-",array[4],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
}


if(length(array)==5){
        v=venn.diagram(ven, filename=NULL, fill=c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),  margin=0.15,cex=1.2,cat.cex=1.4,scaled=F,lty=0,cat.dist=c(0.2,0.25,0.2,0.2,0.25),cat.pos=c(0,-20,-160,160,20),fontfamily="sans", cat.fontfamily="sans",alpha=0.4)
        grid.newpage()
        pdf(paste(Outdir,comparename,".DEG_Venn_diagram.pdf",sep=""))
        grid.draw(v)
        dev.off()
	png(paste(Outdir,comparename,".DEG_Venn_diagram.png",sep=""),type="cairo-png")
        grid.draw(v)
        dev.off()
	genelist=unique(c(ven[[1]],ven[[2]],ven[[3]],ven[[4]],ven[[5]]))
	yes_yes_yes_yes_yes<-intersect(intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[4]])),ven[[5]])
	yes_yes_yes_yes_no<-intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[4]]))[!( intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[4]])) %in% ven[[5]])]
	yes_yes_yes_no_yes<-intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[5]]))[!( intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[3]],ven[[5]])) %in% ven[[4]])]
	yes_yes_no_yes_yes<-intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[4]],ven[[5]]))[!( intersect(intersect(ven[[1]],ven[[2]]),intersect(ven[[4]],ven[[5]])) %in% ven[[3]])]
	yes_no_yes_yes_yes<-intersect(intersect(ven[[1]],ven[[3]]),intersect(ven[[4]],ven[[5]]))[!( intersect(intersect(ven[[1]],ven[[3]]),intersect(ven[[4]],ven[[5]])) %in% ven[[2]])]
	no_yes_yes_yes_yes<-intersect(intersect(ven[[2]],ven[[3]]),intersect(ven[[4]],ven[[5]]))[!( intersect(intersect(ven[[2]],ven[[3]]),intersect(ven[[4]],ven[[5]])) %in% ven[[1]])]
	yes_yes_yes_no_no<-intersect(intersect(ven[[1]],ven[[2]]),ven[[3]])[! (intersect(intersect(ven[[1]],ven[[2]]),ven[[3]]) %in% c(ven[[4]],ven[[5]]) ) ]
	yes_yes_no_yes_no<-intersect(intersect(ven[[1]],ven[[2]]),ven[[4]])[! (intersect(intersect(ven[[1]],ven[[2]]),ven[[4]]) %in% c(ven[[3]],ven[[5]]) ) ]
	yes_no_yes_yes_no<-intersect(intersect(ven[[1]],ven[[3]]),ven[[4]])[! (intersect(intersect(ven[[1]],ven[[3]]),ven[[4]]) %in% c(ven[[2]],ven[[5]]) ) ]
	no_yes_yes_yes_no<-intersect(intersect(ven[[2]],ven[[3]]),ven[[4]])[! (intersect(intersect(ven[[2]],ven[[3]]),ven[[4]]) %in% c(ven[[1]],ven[[5]]) ) ]
	yes_yes_no_no_yes<-intersect(intersect(ven[[1]],ven[[2]]),ven[[5]])[! (intersect(intersect(ven[[1]],ven[[2]]),ven[[5]]) %in% c(ven[[3]],ven[[4]]) ) ]
	yes_no_yes_no_yes<-intersect(intersect(ven[[1]],ven[[3]]),ven[[5]])[! (intersect(intersect(ven[[1]],ven[[3]]),ven[[5]]) %in% c(ven[[2]],ven[[4]]) ) ]
	no_yes_yes_no_yes<-intersect(intersect(ven[[2]],ven[[3]]),ven[[5]])[! (intersect(intersect(ven[[2]],ven[[3]]),ven[[5]]) %in% c(ven[[1]],ven[[4]]) ) ]
	yes_no_no_yes_yes<-intersect(intersect(ven[[1]],ven[[4]]),ven[[5]])[! (intersect(intersect(ven[[1]],ven[[4]]),ven[[5]]) %in% c(ven[[2]],ven[[3]]) ) ]
	no_yes_no_yes_yes<-intersect(intersect(ven[[2]],ven[[4]]),ven[[5]])[! (intersect(intersect(ven[[2]],ven[[4]]),ven[[5]]) %in% c(ven[[1]],ven[[3]]) ) ]
	no_no_yes_yes_yes<-intersect(intersect(ven[[3]],ven[[4]]),ven[[5]])[! (intersect(intersect(ven[[3]],ven[[4]]),ven[[5]]) %in% c(ven[[1]],ven[[2]]) ) ]
	yes_yes_no_no_no<-intersect(ven[[1]],ven[[2]])[! (intersect(ven[[1]],ven[[2]]) %in% c(ven[[3]],ven[[4]],ven[[5]]))]
	yes_no_yes_no_no<-intersect(ven[[1]],ven[[3]])[! (intersect(ven[[1]],ven[[3]]) %in% c(ven[[2]],ven[[4]],ven[[5]]))]
	no_yes_yes_no_no<-intersect(ven[[2]],ven[[3]])[! (intersect(ven[[2]],ven[[3]]) %in% c(ven[[1]],ven[[4]],ven[[5]]))]
	yes_no_no_yes_no<-intersect(ven[[1]],ven[[4]])[! (intersect(ven[[1]],ven[[4]]) %in% c(ven[[2]],ven[[3]],ven[[5]]))]
	no_yes_no_yes_no<-intersect(ven[[2]],ven[[4]])[! (intersect(ven[[2]],ven[[4]]) %in% c(ven[[1]],ven[[3]],ven[[5]]))]
	no_no_yes_yes_no<-intersect(ven[[3]],ven[[4]])[! (intersect(ven[[3]],ven[[4]]) %in% c(ven[[1]],ven[[2]],ven[[5]]))]
	yes_no_no_no_yes<-intersect(ven[[1]],ven[[5]])[! (intersect(ven[[1]],ven[[5]]) %in% c(ven[[2]],ven[[3]],ven[[4]]))]
	no_yes_no_no_yes<-intersect(ven[[2]],ven[[5]])[! (intersect(ven[[2]],ven[[5]]) %in% c(ven[[1]],ven[[3]],ven[[4]]))]
	no_no_yes_no_yes<-intersect(ven[[3]],ven[[5]])[! (intersect(ven[[3]],ven[[5]]) %in% c(ven[[1]],ven[[2]],ven[[4]]))]
	no_no_no_yes_yes<-intersect(ven[[4]],ven[[5]])[! (intersect(ven[[4]],ven[[5]]) %in% c(ven[[1]],ven[[2]],ven[[3]]))]
	yes_no_no_no_no<-ven[[1]][! (ven[[1]] %in% c(ven[[2]],ven[[3]],ven[[4]],ven[[5]]))]
	no_yes_no_no_no<-ven[[2]][! (ven[[2]] %in% c(ven[[1]],ven[[3]],ven[[4]],ven[[5]]))]
	no_no_yes_no_no<-ven[[3]][! (ven[[3]] %in% c(ven[[1]],ven[[2]],ven[[4]],ven[[5]]))]
	no_no_no_yes_no<-ven[[4]][! (ven[[4]] %in% c(ven[[1]],ven[[2]],ven[[3]],ven[[5]]))]
	no_no_no_no_yes<-ven[[5]][! (ven[[5]] %in% c(ven[[1]],ven[[2]],ven[[3]],ven[[4]]))]
	write.table(as.data.frame(genelist),paste(Outdir,"all.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_no_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
        write.table(as.data.frame(yes_no_yes_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_yes_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_yes_no_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-yes-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_no_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_yes_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_yes_yes_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_no_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_yes_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_yes_no_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_no_yes_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_no_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_yes_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_yes_no_no_no),paste(Outdir,array[1],"-yes-",array[2],"-yes-",array[3],"-no-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_yes_no_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-yes-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_yes_no_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-yes-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_no_yes_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_no_yes_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_yes_yes_no),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_no_no_yes),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_no_no_yes),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_yes_no_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_no_yes_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-no-",array[4],"-yes-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(yes_no_no_no_no),paste(Outdir,array[1],"-yes-",array[2],"-no-",array[3],"-no-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_yes_no_no_no),paste(Outdir,array[1],"-no-",array[2],"-yes-",array[3],"-no-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_yes_no_no),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-yes-",array[4],"-no-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_no_yes_no),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-no-",array[4],"-yes-",array[5],"-no.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
	write.table(as.data.frame(no_no_no_no_yes),paste(Outdir,array[1],"-no-",array[2],"-no-",array[3],"-no-",array[4],"-no-",array[5],"-yes.txt",sep=""),col.names=F,row.names=F,quote=F,sep="\t")
}
