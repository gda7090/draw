library(argparse)
parser = ArgumentParser()
library(reshape2)
library(Matrix)
library('ggplot2')
#parser$add_argument("--tsne", help="the tsne.csv of cellranger analysis",required=TRUE)
#parser$add_argument("--clusterpath",help='the clusterpath of cluster*.xls file',required=TRUE)
#parser$add_argument("--sample", help='sample name',required=TRUE)

#args <- parser$parse_args()
#str(args)

clusterpath = "/batchTSNE/cellranger3.0/aggr/EvsLvsAmvsOld/outs/analysis/clustering"
sample = "3.0test" 
tsne= "/batchTSNE/cellranger3.0/aggr/EvsLvsAmvsOld/outs/analysis/tsne/2_components/projection.csv"
libcsv="EvsLvsAmvsOld.lib.csv"

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


projection <- read.csv(tsne,header=T,sep=',',row.names=1)
## kmeans TSNE
dirs <- list.files(path=clusterpath)
datalist <- lapply(dirs, function(dir) {read.table(paste(clusterpath,dir,'clusters.csv',sep='/'),sep=',',header=T)})
names(datalist)<-dirs
n_clu <- 2:10
km_all<-sapply(n_clu, function(x) datalist[[paste("kmeans",x,"clusters",sep="_")]]$Cluster)
colnames(km_all)<- sapply(n_clu, function(x) paste("kmeans",x,sep="."))
filename<-paste(sample,"allkmeans_clustering.pdf",sep='_')
pdf(file=paste(outdir,filename,sep='/'))
p<-visualize_clusters(km_all,projection[c("TSNE.1","TSNE.2")],title="kmeans clustering labels")
print(p)
dev.off()
for(n in n_clu){
    clu<-datalist[[paste("kmeans",n,"clusters",sep="_")]]$Cluster
    p<-visualize_clusters(clu,projection[c("TSNE.1","TSNE.2")],title="kmeans clustering labels")
    filename<-paste(sample,"kmeans",n,"clustering.pdf",sep='_')
    km_dir<-paste("kmeans",n,"clusters",sep="_")
    km_dir<-paste(outdir,km_dir,sep="/")
    dir.create(km_dir)
    pdf(file=paste(km_dir,filename,sep="/"))
    print(p)
    dev.off()
}

## graphclust TSNE
graph_clust<-datalist[["graphclust"]]
filename=paste(sample,"graphbased_clustering.pdf",sep='_')
graph_dir=paste(outdir,"graphclust",sep='/')
dir.create(graph_dir)
pdf(file=paste(graph_dir,filename,sep='/'))
p<-visualize_clusters(graph_clust$Cluster,projection[c("TSNE.1","TSNE.2")],title="Graph-based clustering labels")
print(p)
dev.off()
save(list =ls(all=TRUE), file="3.0test.RData") 
## multisample TSNE
if (file.exists(libcsv)){
libinfo<-read.csv(libcsv,sep=',',header=T)
samplelabel<-as.factor(libinfo$library_id)
mergeID<-substr(projection$Barcode,18,19)
IDstat<-unlist(lapply(1:length(samplelabel), function(x) sum(mergeID==x)))
mergerdsamplelabel<-unlist(lapply(1:length(samplelabel), function(x) rep(samplelabel[x],IDstat[x])))
colsNum<-sort(unique(mergeID))
cols<-c("red","blue","lightseagreen","orange","mediumpurple","palegreen","lightcoral","dodgerblue","lawngreen","yellow","salmon","mediumslateblue","darkviolet","purple","sienna","tan","chocolate","skyblue","turquoise","cadetblue","silver","aliceblue","black")
cols<-cols[1:length(colsNum)]
filename=paste(sample,"multisample.pdf",sep='_')
pdf(file=paste(outdir,filename,sep='/'))
p<-vis_clusters(mergerdsamplelabel,projection[c("TSNE.1","TSNE.2")],title="batch labels",colour=cols)
print(p)
dev.off()
}else{
#"提示没有libcsv输入所以不能做该图并退出"
}















