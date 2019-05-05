library(cellrangerRkit)
library(argparse)
library(reshape2)
parser = ArgumentParser()
parser$add_argument("--cellranger_path", help="/path/to/cellranger/output/directory",required=TRUE)
parser$add_argument("--sample", help='sample name, if Aggr name(Sample1vsSample2vsSample3...) and manually set(not get from the pipeline), the order of the samples in the aggr name must be the same with that in the compare file or that in the lib.csv.',required=TRUE) 
parser$add_argument("--type", help='type of tsne picture, [kmeans,graphclust,multisample,all]',default="all")
parser$add_argument("--outdir", help='the output directory',default=".")


args <- parser$parse_args()
str(args)
cellranger_pipestance_path = args$cellranger_path
sample = args$sample
type = args$type
outdir = args$outdir
dir.create(outdir,recursive = TRUE) 

analysis_results <- load_cellranger_analysis_results(cellranger_pipestance_path)
gbm <- load_cellranger_matrix(cellranger_pipestance_path)
#use_genes <- get_nonzero_genes(gbm)
#gbm_bcnorm <- normalize_barcode_sums_to_median(gbm[use_genes,])
#gbm_log <- log_gene_bc_matrix(gbm_bcnorm,base=10)
tsne_proj <- analysis_results$tsne
save(list =ls(all=TRUE), file=paste(outdir,"temp.RData1",sep="/"))

#"crimson" "olive"
#cols<-c("1"="red","2"="blue", "3"="lightseagreen", "4"="orange", "5"="mediumpurple", "6"="palegreen")
#example_col<-rainbow(23)

vis_clusters<-function (cluster_result, projection, colour = NULL, alpha = 1,
    marker_size = 0.1, title = NULL, legend_anno = NULL)
{
    cluster_ids <- unique(as.vector(cluster_result))
    if (is.vector(cluster_result)) {
        if (dim(projection)[1] != length(cluster_result))
            stop("The number of labels and the number of projected points do not match!\n")
    }
    else {
        if (dim(projection)[1] != dim(cluster_result)[1]) {
            stop("The number of labels and the number of projected points do not match!\n")
        }
    }
    if (!is.null(legend_anno)) {
        if (length(legend_anno) != length(cluster_ids))
            stop("The length of legend_anno is not the same as the number of unique labels!\n")
    }
    if (!is.null(colour)) {
        if (length(colour) != length(cluster_ids))
            stop("The length of colour is not the same as the number of unique labels!\n")
        names(colour) <- cluster_ids
        if (!is.null(legend_anno)) {
            names(colour) <- legend_anno
        }
    }
    projection_names <- colnames(projection)
    colnames(projection) <- c("Component.1", "Component.2")
    proj_clu <- data.frame(cbind(projection, cluster_result))
    proj_clu_melt <- melt(proj_clu, id.vars = c("Component.1",
        "Component.2"))
    if (!is.null(legend_anno)) {
        cid_idx <- unlist(lapply(cluster_result, function(x) which(cluster_ids %in%
            x)))
        proj_clu_melt$value <- factor(legend_anno[cid_idx])
    }
    else {
        proj_clu_melt$value <- factor(proj_clu_melt$value)
    }
    p <- ggplot(proj_clu_melt, aes(Component.1, Component.2)) +
        geom_point(aes(colour = value), size = marker_size, alpha = alpha) +
        facet_wrap(~variable) + guides(col = guide_legend(title = "ID",
        override.aes = list(size = 3))) + labs(x = projection_names[1],
        y = projection_names[2]) + theme_bw()
    if (!is.null(title)) {
        p <- p + ggtitle(title)
    }
    if ((!is.null(colour))) {
        p <- p + scale_color_manual(values = colour)
    }
    if (is.vector(cluster_result)) {
        p <- p + theme(plot.title = element_text(hjust = 0.5),
            legend.key = element_blank(), strip.text.x = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }
    else {
        p <- p + theme(plot.title = element_text(hjust = 0.5),
            legend.key = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }
    return(p)
}

if((type == "all") || (type=="kmeans")){
n_clu <- 2:10
km_res <- analysis_results$clustering # load pre-computed kmeans results
clu_res <- sapply(n_clu, function(x) km_res[[paste("kmeans",x,"clusters",sep="_")]]$Cluster) 
colnames(clu_res) <- sapply(n_clu, function(x) paste("kmeans",x,sep=".")) 
filename<-paste(sample,"allkmeans_clustering.pdf",sep='_')
pdf(file=paste(outdir,filename,sep='/'))
visualize_clusters(clu_res,tsne_proj[c("TSNE.1","TSNE.2")],title="kmeans clustering labels")
dev.off()
for(n in n_clu){
    clu<-km_res[[paste("kmeans",n,"clusters",sep="_")]]$Cluster
    p<-visualize_clusters(clu,tsne_proj[c("TSNE.1","TSNE.2")],title="kmeans clustering labels")
    filename<-paste(sample,"kmeans",n,"clustering.pdf",sep='_')
    km_dir<-paste("kmeans",n,"clusters",sep="_")
    km_dir<-paste(outdir,km_dir,sep="/")
    dir.create(km_dir)
    pdf(file=paste(km_dir,filename,sep="/"))
    print(p)
    dev.off()
}
}

if((type == "all") || (type=="graphclust")){
cluster_result <- analysis_results$clustering[["graphclust"]]
filename=paste(sample,"graphbased_clustering.pdf",sep='_')
graph_dir=paste(outdir,"graphclust",sep='/')
dir.create(graph_dir)
pdf(file=paste(graph_dir,filename,sep='/'))
visualize_clusters(cluster_result$Cluster,tsne_proj[c("TSNE.1","TSNE.2")],title="Graph-based clustering labels")
}
dev.off()

if((type == "all") || (type=="multisample")){
#mergeID<-substr(pData(gbm)$barcode,18,19)
mergeID<-substr(tsne_proj$Barcode,18,19)
samplelabel<-unlist(strsplit(sample, "vs", fixed = TRUE))
#IDstat<-as.vector(table(mergeID))
#IDstat<-IDstat[c(1,4,5,6,7,8,9,10,11,2,3)]
IDstat<-unlist(lapply(1:length(samplelabel), function(x) sum(mergeID==x)))
mergerdsamplelabel<-unlist(lapply(1:length(samplelabel), function(x) rep(samplelabel[x],IDstat[x])))
colsNum<-sort(unique(mergeID))
cols<-c("red","blue","lightseagreen","orange","mediumpurple","palegreen","lightcoral","dodgerblue","lawngreen","yellow","salmon","mediumslateblue","darkviolet","purple","sienna","tan","chocolate","skyblue","turquoise","cadetblue","silver","aliceblue","black")
cols<-cols[1:length(colsNum)]
#cols<-c("E"="red","L"="blue","old_1"="lightseagreen","old_2"="orange","old_3"="mediumpurple","PG_1"="palegreen","PG_2"="lightcoral","Af_1"="dodgerblue","Af_2"="lawngreen","Af_3"="yellow","Af_4"="salmon")
filename=paste(sample,"multisample.pdf",sep='_')
pdf(file=paste(outdir,filename,sep='/'))
#visualize_clusters(mergerdsamplelabel,tsne_proj[c("TSNE.1","TSNE.2")],title="batch labels",colour=cols)
vis_clusters(mergerdsamplelabel,tsne_proj[c("TSNE.1","TSNE.2")],title="batch labels",colour=cols)
}
dev.off()
