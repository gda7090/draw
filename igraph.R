library("igraph")
library(ggplot2)
set.seed(123)
fplot=function(file){
	#=== add triangle to shape
	mytriangle <- function(coords, v=NULL, params) {
		vertex.color <- params("vertex", "color")
	    	if (length(vertex.color) != 1 && !is.null(v)) {
	        	vertex.color <- vertex.color[v]
			vertex.frame.color=NA
		}
		vertex.size <- 1/200 * params("vertex", "size")
		if (length(vertex.size) != 1 && !is.null(v)) {
			vertex.size <- vertex.size[v]
		}
		vertex.frame.color=NA
		symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
			stars=cbind(vertex.size, vertex.size, vertex.size),
			add=TRUE, inches=FALSE)
	}
	add_shape("triangle", clip=shapes("circle")$clip,plot=mytriangle)

	#============================================
	df=read.table(file,header=F)
	df1=data.frame(from=df[,1],to=df[,3])
	g=graph.data.frame(df1,directed=TRUE)
	V(g)$color="green"
	V(g)[as.vector(df[,1][df[,2]=="up"])]$color="red"
	V(g)[as.vector(df[,3][df[,5]=="up"])]$color="red"
	V(g)$shape="triangle"
	V(g)[as.vector(df[,1])]$shape="circle"
	V(g)[as.vector(df[,3][df[,4]=="gene"])]$shape="square"
	if (length(unique(df[,1]))<3){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=10,vertex.label.cex=0.3)
	}else if(length(unique(df[,1]))<=6){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=6,vertex.label.cex=0.3)
	}else if(length(unique(df[,1]))<=9){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=4,vertex.label.cex=0.3)
	}else{
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.02,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=4,vertex.label.cex=0.3)
	}
#	title(main=name,line=-1)
	legend(x=1.1,y=-0.9, pch=c(21,22,24),c("miRNA","gene","lnc"),cex=0.8,pt.cex=1.5)
	legend(x=1.1,y=-1.2, fill=c("red","green"),c("up","down"),cex=0.8)
}

fplot2=function(file,type){
	df=read.table(file,header=F)
	df[,3]=factor(df[,3])
	df1=data.frame(from=df[,1],to=df[,3])
	g=graph.data.frame(df1,directed=TRUE)
	V(g)$color="green"
	V(g)[as.vector(df[,1][df[,2]=="up"])]$color="red"
	V(g)[as.vector(df[,3][df[,4]=="up"])]$color="red"
	V(g)$shape="square"
	V(g)[as.vector(df[,1])]$shape="circle"
	if (length(unique(df[,1]))<3){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=10,vertex.label.cex=0.3)
	}else if(length(unique(df[,1]))<=6){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=6,vertex.label.cex=0.3)
	}else if(length(unique(df[,1]))<=9){
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=4,vertex.label.cex=0.3)
	}else{
		plot(g, layout=layout.fruchterman.reingold,edge.arrow.size=0.02,vertex.shape=V(g)$shape,vertex.color=V(g)$color,vertex.size=4,vertex.label.cex=0.3,)
	}
	legend(x=1.1,y=-0.9, pch=c(21,22),c("miRNA",type),cex=0.8,pt.cex=1.5)
	legend(x=1.1,y=-1.2, fill=c("red","green"),c("up","down"),cex=0.8)
}
fplot3=function(file){
	df=read.table(file,header=T)
	ggplot(df,aes(x=type,y=count,fill=updown))+geom_bar(position="dodge",stat="identity")
}
