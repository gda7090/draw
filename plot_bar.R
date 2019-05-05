#!miniconda3/lib/R/bin/R
args<-commandArgs(T)
if(length(args)!=3){
  cat ("[usage:] <Unigenes.group.relative.s.xls> <out.pdf> <bar position>\n")
  cat ("Example: bar_stack_plot.R   Unigenes.group.relative.s.xls  Unigenes.group.relative.s.pdf [dodge,fill,stack]\n")
  quit("no")
}
clor<-c("red","blue","lightseagreen","orange","mediumpurple","palegreen","lightcoral","dodgerblue","lawngreen","yellow","salmon","mediumslateblue","darkviolet","purple","sienna","tan","chocolate","skyblue","turquoise","cadetblue","aliceblue","black")
#clor<-c("#CC0000","#6666CC","#666600","#000066","#66CC66","#CC0066","#0000CC","#666666","#CC00CC","#006600","#66CCCC","#CC6600","#006666","#CC6666","#0066CC","#660000","#CC66CC","#00CC00","#6600CC","#CCCC00","#00CC66","#660066","#CCCC66","#00CCCC","#66CC00","#000000","#CCCCCC","#79CDCD","#CD5555","#20B2AA","#A2CD5A","#008B45","#008B00","#006400","#CD5555","#CD4F39","#F4A460","#104E8B","#00688B","#5D478B","#4B0082","#EEE685","#7D9EC0","#8DB6CD","#8B668B","#FFFFFF","#C4C4C4")

a<-read.table(args[1],sep="\t",head=T,check.names = F,row.names=1)
#row.names(a)<-gsub("^.__.*;", "",row.names(a))
library(tidyverse)

a <- as.data.frame(a) 
#a<-a[,-ncol(a)];
#colnames(a)<-c("Group1","Group2","Group3");

mt= cbind(rownames(a),a);
colnames(mt)[1]<-"samples";
td <- gather(mt,key="Cluster Name",value="Cells Ratio",-samples)
td[,2]<-factor(td[,2], levels = sort(as.numeric(colnames(a)))) #change the order 
td[,1]<- factor(td[,1], levels = sort(as.numeric(rownames(a))))#as.character(td[,1])
#aes(order=desc(abundance)
plt<- ggplot(td,aes(x=td[,1],y=td[,3],fill=td[,2]))+
    geom_bar(position = args[3],stat="identity")+
    labs(x="Cluster Name",y="Cells Ratio")+
    theme(panel.background=element_rect(fill='transparent', color='gray'),
	      legend.key=element_rect(fill='transparent', color='transparent'),
		  axis.text.x=element_text(angle=90,hjust=1,vjust=1)
		  #plot.margin = unit(c(0.5,1,0.5,1),"cm")
		  )+
	scale_y_continuous(expand=c(0.001,0.001))+
	#theme_classic()+
	#theme_minimal()+ 
	#theme_bw()+
	#theme(axis.text.x=element_text(angle=0,hjust=1), legend.text = element_text(size = 8, colour = "black", angle = 0))+
    scale_fill_manual(values=clor)+
    guides(fill = guide_legend(keywidth = 0.5, keyheight = 1,ncol=1,title = NULL))
ggsave(args[2], plt, scale=1)
