### generate all sample SNP/INDEL count program : vcf_counts.py
#.libPaths(new='lib/R')
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(grid)
args = commandArgs(TRUE)
if(length(args) != 2 ){
   print("Usage: Rscript SNP_plot_3.R <data_dir> <png_output_dir>")
   print("e.g. : Rscript SNP_plot_3.R data.example 3")
   stop()
}
read_data_dir = args[1]
output_png_dir = args[2]

#theme
theme_bare <- theme(
  title = element_blank(),
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  legend.key.size = unit(0.8, "cm"),
  legend.text = element_text(size = 13)
)

### indel_exonicfunc.count  
### indel_func.count
### snp_exonicfunc.count
### snp_func.count
#opar=par(mar=c(5, 4, 4, 2) + 0.1 , mfrow=c(1,1))
file_name = c('indel_function.stat.xls','snp_function.stat.xls')

### indel
all_data_file = paste( read_data_dir , file_name[1] , sep = "/")
all_data = read.table(all_data_file,sep='\t',head=TRUE,check.names=F)
rownames(all_data) = all_data[,1]
sample_names = colnames(all_data)
#all_data_1 = all_data[c(1,11,12,13,14,15,16,17,18,19,20,21,22,23),]
#all_data_2 = all_data[c(2,3,4,5,6,7,8,9,10),]
all_data_1 = all_data[c(1,8,9,10,11,12,13,14,15,16,17,18,19,20),]
all_data_2 = all_data[c(2,3,4,5,6,7),]
plot_label_1 = rownames(all_data_1)
plot_label_2 = rownames(all_data_2)
png_name = "indel."
for( i in 2:ncol(all_data) ){
      png_file = paste(output_png_dir,'/',sample_names[i],".",png_name,"png",sep="")           
      png( png_file , width = 1300 , height = 650 ,type="cairo-png")
	  df = data.frame(all_data_1[ ,i ])
      sample_data = as.numeric(all_data_1[ ,i ])
	 
	  p1<-ggplot(df, aes(x=1,y=sample_data,fill=plot_label_1))+
	  geom_bar(stat='identity') +
	  coord_polar(theta="y") +
	  xlab("") + ylab("")  +
	  theme_bare

		df2 = data.frame(all_data_2[ ,i ])
       sample_data1 = as.numeric(all_data_2[ ,i ])
	   p2<-ggplot(df2,aes(x=1,y=sample_data1,fill=plot_label_2))+	
	   geom_bar(stat='identity') +
	   coord_polar(theta="y") +
	   xlab("") + ylab("")  +
	   theme_bare

	  grid.arrange(p1, p2, ncol=2, main = textGrob(sample_names[i], vjust = 2, hjust = 1, gp = gpar(fontface = "bold", cex = 2)))
      dev.off()      
}

### snp
all_data_file = paste( read_data_dir , file_name[2] , sep = "/")
all_data  = read.table( all_data_file , sep = "\t" , header = TRUE, check.names=F) 
rownames(all_data) = all_data[,1]
sample_names = colnames(all_data)
#all_data_1 = all_data[c(1,7,8,9,10,11,12,13,14,15,16,17,18,19),]
#all_data_2 = all_data[c(2,3,4,5),]
all_data_1 = all_data[c(1,7,8,9,10,11,12,13,14,15,16,17,18,19),]
all_data_2 = all_data[c(2,3,4,5,6),]
plot_label_1 = rownames(all_data_1)
plot_label_2 = rownames(all_data_2)

png_name = "snp."
for( i in 2:ncol(all_data) ){
      png_file = paste(output_png_dir,'/',sample_names[i],".",png_name,"png",sep="")
      png( png_file , width = 1300 , height = 650 ,type="cairo-png")

	  df = data.frame(all_data_1[ ,i ])
      sample_data = as.numeric(all_data_1[ ,i ])
	  p1<-ggplot(df, aes(x=1,y=sample_data,fill=plot_label_1))+
	  geom_bar(stat='identity') +
	  coord_polar(theta="y") +
	  xlab("") + ylab("")  +
	  theme_bare

	df2 = data.frame(all_data_2[ ,i ])
	sample_data2 = as.numeric(all_data_2[ ,i ])
	p2<-ggplot(df2,aes(x=1,y=sample_data2,fill=plot_label_2))+	
	geom_bar(stat='identity') +
	coord_polar(theta="y") +
	xlab("") + ylab("")  +
	theme_bare
		   
	grid.arrange(p1, p2, ncol=2, main = textGrob(sample_names[i], vjust = 2, hjust = 1, gp = gpar(fontface = "bold", cex = 2)))

	dev.off()     


}

