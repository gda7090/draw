library(stringr)
library(methods)
#.libPaths(new="")
.libPaths(new='../')
library(argparser)
p <- arg.parser("plot diff genes on chr plot")
p <- add.argument(p,"--geneInfo", help="geneInfo file for TR")
p <- add.argument(p,"--diff_gene_xls", help="diffgene.xls file")
p <- add.argument(p,"--fai",help="fai file for chromosome length")
p <- add.argument(p,"--title",help='title for pic output')
p <- add.argument(p,"--subchr",help="if you don't want all chromosome, then use this comma separated string")
p <- add.argument(p,"--width",help="width in cm for figure output",default=12)
p <- add.argument(p,"--outdir",help='output directory')
p <- add.argument(p,"--genes",help="mRNA or lncRNA list")
args = parse.args(p, argv = commandArgs(trailingOnly = TRUE))

#print(args)

geneInfo = args$geneInfo
diff_gene_xls = args$diff_gene_xls
fai = args$fai
final_title = args$title
outdir = args$outdir
diff_list=args$genes

if(is.null(args$subchr)){
    subset.chr = NULL
}else{
    subset.chr = unlist(strsplit(args$subchr,split=','))
}

if(!exists("geneInfo")){
    geneInfo="after_zcat/Diff_TR/Diff/geneInfo"
    }
if(!exists("diff_gene_xls")){
    diff_gene_xls="after_zcat/Diff_TR/Diff/CG31917vsControl/CG31917vsControl.diffgene.xls"
    }
if(!exists("fai")){
    fai="after_zcat/fruitfly.fa.fai"
}
if(!exists("subset.chr")){
   subset.chr = c("2L","2R","3L","3R","X")
}


op.old <- options(stringsAsFactors = FALSE)

df.info = read.table(geneInfo, header=T, sep="\t")
df.diff = read.table(diff_gene_xls, header=T, sep="\t")
df.fai = read.table(fai, header=F, sep="\t")

stopifnot(sum(df.diff$pvalue >= 0.05)==0) # all entry should be siginificant

df.diff = df.diff[,c("transcript_id","gene_id","log2.foldchange.")]

df.merge <- merge(df.diff, df.info, by.x="gene_id", by.y="gene_id")
message(str(df.merge))

library(plyr)
#ddply(df.merge,.(chromosome),nrow)

if(!is.null(subset.chr)){
    df.merge <- subset(df.merge, chromosome %in% subset.chr)[,c("transcript_id","log2.foldchange.","chromosome","start","end","length")]
    if (args$genes != "all"){
        diff_list=args$genes
	diff_list
        filter_list<-read.table(diff_list,head=T)
        filter_list<-as.matrix(filter_list)
	head(filter_list)
	df.merge<-df.merge[(df.merge$transcript_id %in% filter_list[,1]),]
	}
}else{
    subset.chr = unique(df.merge$chromosome)
}                                       # after this, subset.chr is stuffed!
head(df.merge)
#dim(df.merge)
df.merge$pos = (df.merge$start+df.merge$end)/2

#write.table(df.merge,"df.merge.xls",sep="\t",quote=F,row.names=F)

## after this df.merge file
df.merge = df.merge[,c("chromosome","pos","log2.foldchange.")]
df.merge$chromosome = factor(df.merge$chromosome, levels=subset.chr)
df.fai = subset(df.fai, df.fai[,1] %in% subset.chr)[,c(1,2)]
names(df.fai) <- c("chromosome","length")
df.fai$chromosome = factor(df.fai$chromosome,levels=levels(df.merge$chromosome))

df.merge2 <- ddply(df.merge,.(chromosome),function(df){
    x <- xend <- df['pos']
    y <- 0
    yend <- df['log2.foldchange.']
    updown <- sapply(df['log2.foldchange.'] < 0,function(x) {if(x)"Down"else"Up"})
    data.frame(x=x,xend=xend,y=y,yend=yend,updown=updown)
})
names(df.merge2) <- c("chromosome","x","xend","y","yend","updown")
df.merge2$updown <- factor(df.merge2$updown,levels=c("Up",'Down'))

library(ggplot2)

p <- ggplot(df.merge2,aes(x=chromosome,color=updown)) +
    geom_segment(aes(x=x,xend=xend,y=y,yend=yend),alpha=0.8,size=1.5) + facet_grid(chromosome~.) +
        theme_classic() + theme(panel.grid=element_blank(),
                                   panel.border=element_blank(),
                                   strip.background=element_blank(),
								   strip.text = element_text(angle = 90)) +
                                       ylab("log2FoldChange") + xlab("position (bp)") +
                                           guides(color=guide_legend(title=NULL)) +
                                               scale_color_manual(values=c("Up"="red","Down"="blue")) +
                                                   geom_segment(aes(x=0,xend=length,y=0,yend=0),data=df.fai,inherit.aes=FALSE)

if(!is.null(final_title)){
    p <- p + labs(title=final_title)
}

library(grid)
width = args$width
pdf(paste(outdir,"/",final_title,"_diff_distribution.pdf",sep=""),width=unit(width,"cm"),height=unit(length(subset.chr)*2,"cm"))
print(p)
dev.off()

#res = 300
png(paste(outdir,"/",final_title,"_diff_distribution.png",sep=""),type='cairo-png',width=600,height=length(subset.chr)*80)
#png(paste(outdir,"/",final_title,"_diff_distribution.png",sep=""),type='cairo-png',width=unit(width,"cm"),height=unit(length(subset.chr),"cm"))
print(p)
dev.off()
