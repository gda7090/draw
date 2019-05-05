library(argparse)
parser = ArgumentParser()
library(reshape2)
library(Matrix)
library('ggplot2')
#parser$add_argument("--cellranger_path", help="/path/to/cellranger/output/directory",required=TRUE)
parser$add_argument("--matrix_dir", help="the filtered_feature_bc_matrix dir of cellranger analysis",required=TRUE)
parser$add_argument("--tsne", help="the tsne.csv of cellranger analysis",required=TRUE)
parser$add_argument("--path",help='the path of cluster*.xls file',required=TRUE)
parser$add_argument("--sample", help='sample name',required=TRUE)

args <- parser$parse_args()
str(args)

matrix_dir = args$matrix_dir
path = args$path
sample = args$sample
tsne= args$tsne

barcode.path <- paste0(matrix_dir, "/barcodes.tsv.gz")
features.path <- paste0(matrix_dir, "/features.tsv.gz")
matrix.path <- paste0(matrix_dir, "/matrix.mtx.gz")
mat <- readMM(file = matrix.path)

feature.names = read.delim(features.path,
                           header = FALSE,
                           stringsAsFactors = FALSE)
barcode.names = read.delim(barcode.path,
                           header = FALSE,
                           stringsAsFactors = FALSE)
colnames(mat) = barcode.names$V1
rownames(mat) = feature.names$V2

#index<-duplicated(rownames(mat))
#mat<-mat[!index,]

###filtered unexpression genes
#bc_counts <- sort(rowSums(mat), decreasing=TRUE)
bc_counts <- rowSums(mat)
mat_1<-mat[bc_counts>0,]

###normolize UMI count and log-transformed
bc_sums <- colSums(mat_1)
median_sum <- median(bc_sums)
new_matrix <- sweep(mat_1, 2, median_sum/bc_sums, '*')
log_new_matirx<-log(1+new_matrix, 10)



#library('cellrangerRkit')
##cellranger_pipestance_path <-cellranger_pipestance_path
#analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
#gbm <- load_cellranger_matrix(cellranger_pipestance_path)
##tsne_proj <- analysis_results$tsne
#use_genes <- get_nonzero_genes(gbm)
#gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes,])
#gbm_log <- log_gene_bc_matrix(gbm_bcnorm,base=10)
#tsne_proj <- analysis_results$tsne

path=path
files <- list.files(path=path, pattern="*.xls")
datalist <- lapply(files, function(name) {as.matrix(read.table(paste(path,name,sep='/'),sep='\t',header=T))})

datalist2 = do.call(rbind,datalist)
data<-datalist2[order(datalist2[,4],decreasing=T),]

if (length(data[,2])!=0){
	if (length(data[,2])>=9){
		genes<-data[,2][c(1:9)]
		}
	else if(length(data[,2])<9){
		genes<-data[,2]
		}
	}

#cluster<-read.table(cluster,header=T,sep='\t')
#cluster_1<-cluster[order(cluster[,5]),]
#genes<-as.character(cluster_1[,2][c(1:3)])

##pdf(paste(cluster_label,'_gene.tsne.pdf',sep=''),width = 9, height = 5)

gbm_trunc<-log_new_matirx[genes,]

gene_values <- t(as.matrix(gbm_trunc))
limits=c(0,10)
gene_values[gene_values<limits[1]] <- limits[1]
gene_values[gene_values>limits[2]] <- limits[2]

projection <- read.csv(tsne,header=T,sep=',',row.names=1)

proj_gene <- data.frame(cbind(projection,gene_values))

proj_gene_melt <- melt(proj_gene, id.vars=c("TSNE.1", "TSNE.2"))

p<- ggplot(proj_gene_melt, aes(TSNE.1, TSNE.2)) +
    geom_point(aes(colour=value),size=0.1) + facet_wrap(~variable) +
    scale_colour_gradient(low="grey",high="red",name = "val") +
    labs(x=projection[1],y=projection[2])

p <- p + theme_bw() + theme(plot.title = element_text(hjust = 0.5),
       panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##p
##dev.off()
##p<-visualize_gene_markers(gbm_log,genes,tsne_proj[c("TSNE.1","TSNE.2")])

pdf(paste(sample,'_gene.tsne.pdf',sep=''),width = 9, height = 5)
p
dev.off()

png(paste(sample,'_gene.tsne.png',sep=''),type="cairo-png")
p
dev.off()

svg(paste(sample,'_gene.tsne.svg',sep=''))
p
dev.off()



