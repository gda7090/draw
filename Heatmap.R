library(argparse)
library(scales)

parser = ArgumentParser()
parser$add_argument("--cellranger_path", help="/path/to/cellranger/output/directory",required=TRUE)
parser$add_argument("--cluster", help='the cluster name',required=TRUE)
parser$add_argument("--path",help='the path of cluster*.xls file',required=TRUE)
parser$add_argument("--sample", help='sample name',required=TRUE)

args <- parser$parse_args()
str(args)

library('cellrangerRkit')
library(ggplot2)
library("pheatmap")

sample = args$sample
cluster = args$cluster
path = args$path
cellranger_pipestance_path = args$cellranger_path


gbm <- load_cellranger_matrix(cellranger_pipestance_path)
use_genes <- get_nonzero_genes(gbm)
gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes,])
gbm_log <- log_gene_bc_matrix(gbm_bcnorm,base=10)

files <- list.files(path=path, pattern="*.xls")

ann<-data.frame()
for (i in 1:length(files)) {
        temp = read.csv(paste(path,files[i],sep='/'),header = TRUE,sep='\t')
         if (length(temp[,4])!=0){
                temp2<-temp[order(temp[,4],decreasing=T),]
                clus<-unlist(strsplit(files[i],'.',fixed=TRUE))[1]
                clus_1<-unlist(strsplit(clus,'_',fixed=TRUE))[2]
                if  (length(temp2[,4])>0 && length(temp2[,4])<=3){
                        markers<-as.character(temp2[,1])
                        ann1<-merge(markers,clus_1)
                       ann<-rbind(ann,ann1)
                        }
                else if (length(temp[,1])>3){
                        markers<-as.character(temp[,1])
                        ann1<-merge(markers,clus_1)
                        ann<-rbind(ann,ann1)
                        }
                }
        }
index<-duplicated(ann[,1])
ann_1<-ann[index,][,1]
ann_2<-ann[which(!(ann[,1] %in% ann_1)),]
colnames(ann_2)<-c('geneID','Cluster')

m<-match(ann_2$geneID,rownames(exprs(gbm_log)))
gbm_log_1<-gbm_log[m,]

analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)

cluster_result <- analysis_results$clustering[[cluster]]

c<-unique(ann_2$Cluster)
cluster_result_1<-cluster_result[which(cluster_result$Cluster %in% c),]
cluster_result_2<-cluster_result_1[order(cluster_result_1[,2],decreasing=F),]

n<-match(cluster_result_2$Barcode,colnames(exprs(gbm_log_1)))
gbm_log_2<-gbm_log_1[,n]

data<-exprs(gbm_log_2)
rownames(data)<-fData(gbm_log_2)$symbol

value <- t(scale(t(as.matrix(data))))
limits=c(-3,3)
value[value<limits[1]] <- limits[1]
value[value>limits[2]] <- limits[2]



cluster_result_3<-as.data.frame(factor(cluster_result_2[,2]))
rownames(cluster_result_3)<-cluster_result_2$Barcode
colnames(cluster_result_3)<-'Cluster'
ann_3<-as.data.frame(factor(ann_2[,2]))
rownames(ann_3)<-ann_2$geneID
colnames(ann_3)<-'Cluster'

color = colorRampPalette(rev(c("#D73027", "#D73027", "#FC8D59", "#FFFFBF", "#91BFDB", "#4575B4", "#4575B4")))(1000)

rownames(ann_3)<-rownames(data)



example_K<-length(unique(ann_3$Cluster))

if (example_K>=3 &&example_K <=8) {
        example_col <- rev(brewer.pal(example_K,"Set2"))
}else if (example_K>8 && example_K<=12){
        example_col <- rev(brewer.pal(example_K,"Set3"))
}else if (example_K<3 || example_K>12){
        example_col<-c(hue_pal()(example_K))
}


###example_col <- rev(brewer.pal(example_K,"Set2"))
names(example_col)<-unique(ann_3$Cluster)

ann_colors = list(Cluster= example_col)


##png('heatmap.png',type="cairo-png")

png(paste(sample,'_',cluster,'_heatmap.png',sep=''),type="cairo-png")

pheatmap(value,color=color,annotation = cluster_result_3,annotation_col=cluster_result_3,annotation_row=ann_3,cluster_rows = FALSE,cluster_cols = FALSE, fontsize_row =5, show_colnames = FALSE, show_rownames = FALSE, annotation_colors=ann_colors)

dev.off()


##pdf('heatmap.pdf')

pdf(paste(sample,'_',cluster,'_heatmap.pdf',sep=''),width = 9, height = 5)

pheatmap(value,color=color,annotation = cluster_result_3,annotation_col=cluster_result_3,annotation_row=ann_3,cluster_rows = FALSE,cluster_cols = FALSE, fontsize_row =5, show_colnames = FALSE, show_rownames = FALSE, annotation_colors=ann_colors)

dev.off()


svg(paste(sample,'_',cluster,'_heatmap.svg',sep=''))


pheatmap(value,color=color,annotation = cluster_result_3,annotation_col=cluster_result_3,annotation_row=ann_3,cluster_rows = FALSE,cluster_cols = FALSE, fontsize_row =5, show_colnames = FALSE, show_rownames = FALSE, annotation_colors=ann_colors)

dev.off()





