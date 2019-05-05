#data<-read.table('/NJPROJ2/RNA_S/project_Q3/P101SC18080982-01-B1-18_xiaoshu_20180901/CellRanger/E/E/outs/E_gene_bar.csv',sep=',',header=T,row.names=1)
#data<-data[,-ncol(data)+7:-ncol(data)]
#write.table(data,file='E_gene_bar.filt.csv',row.names=T,col.names=T,quote=F,se=',')
data<-read.table('E_gene_bar.filt.csv',sep=',',header=T,row.names=1)
#save(list =ls(all=TRUE), file="E_gene_bar.filt.RData")
data1<-data[,which(data["ENSMUSG00000069036",]>0)]
#dim(data1)
#[1] 27998     0
#write.table(data1,file='Em_gene_bar.csv',row.names=T,col.names=T,quote=F,se=',')
data2<-data[,!which(data["ENSMUSG00000069036",]==0)]
#write.table(data1,file='Ef_gene_bar.csv',row.names=T,col.names=T,quote=F,se=',')
