# Main qgraph function

qgraph =function( adj, ... )
{

arguments=list(...)

if (length(arguments)>0)
{
for (i in 1:length(arguments))
{
	if (class(arguments[[i]])=="qgraph") 
	{
		if (!is.null(names(arguments[[i]])))
		{
			for (j in 1:length(arguments[[i]]))
			{
				if (!(names(arguments[[i]])[j]%in%names(arguments)))
				{
					arguments[length(arguments)+1]=arguments[[i]][j]
					names(arguments)[length(arguments)]=names(arguments[[i]])[j]
				}
			}
		}
	}
}
}	
# Settings for the edgelist
if(is.null(arguments$edgelist)) 
{
	if (nrow(adj)!=ncol(adj)) edgelist=TRUE else edgelist=FALSE 
} else edgelist=arguments$edgelist
if(is.null(arguments$nNodes)) 
{
	if (edgelist) nNodes=max(c(adj[,1:2])) else nNodes=nrow(adj)
} else nNodes=arguments$nNodes

# Default for fact cut and groups:
if(is.null(arguments$graph)) graph="association" else graph=arguments$graph
if (graph=="factorial") fact=TRUE else fact=FALSE
if (fact & edgelist) stop('Factorial graph needs a correlation matrix')
if (graph=="concentration") partial=TRUE else partial=FALSE
if(is.null(arguments$cut)) 
{
	if (nNodes<50) cut=0 
	if (nNodes>=50 | fact) cut=0.3
} else cut=arguments$cut

if(is.null(arguments$groups)) groups=NULL else groups=arguments$groups

# Factorial graph:
if(is.null(arguments$nfact))
{
 if (is.null(groups)) nfact=round(nNodes/2,0) else nfact=length(groups)
} else nfact=arguments$nfact
 
if (fact)
{
	loadings=loadings(factanal(factors=nfact,covmat=adj,rotation="promax"))

	loadings=loadings[1:nrow(loadings),1:ncol(loadings)]

	loadings[loadings<cut]=0
	loadings[loadings>=cut]=1

	adj=(loadings%*%t(loadings)>0)*1

	diag(adj)=0
}


# SET DEFAULT ARGUMENTS:
# General arguments:
if(is.null(arguments$layout)) layout=NULL else layout=arguments$layout
if(is.null(arguments$maximum)) maximum=0 else maximum=arguments$maximum
if(is.null(arguments$minimum))
{
if (nNodes<50)  minimum=0
if (nNodes>=50)  minimum=0.1
}	else minimum=arguments$minimum
if(is.null(arguments$weighted)) weighted=NULL else weighted=arguments$weighted
if(is.null(arguments$rescale)) rescale=T else rescale=arguments$rescale
if(is.null(arguments$labels)) labels=TRUE else labels=arguments$labels
if(is.null(arguments$edge.labels)) edge.labels=FALSE else edge.labels=arguments$edge.labels
if(is.null(arguments$edge.label.cex)) edge.label.cex=1 else edge.label.cex=arguments$edge.label.cex
if(is.null(arguments$directed))
{
	if (edgelist) directed=FALSE else directed=NULL 
} else directed=arguments$directed
if(is.null(arguments$legend))
{
	if (!is.null(groups) & !is.null(names(groups))) legend=TRUE else legend=FALSE
} else legend=arguments$legend
if(is.null(arguments$plot)) plot=T else plot=arguments$plot
if(is.null(arguments$rotation)) rotation=NULL else rotation=arguments$rotation
if(is.null(arguments$layout.control)) layout.control=0.5 else layout.control=arguments$layout.control
if(is.null(arguments$layout.par)) layout.par=list() else layout.par=arguments$layout.par
if(is.null(arguments$details)) details=TRUE else details=arguments$details

# Output arguments:
if(is.null(arguments$filetype)) filetype="R" else filetype=arguments$filetype
if(is.null(arguments$filename)) filename="qgraph" else filename=arguments$filename
if(is.null(arguments$width)) width=7 else width=arguments$width
if(is.null(arguments$height)) height=7 else height=arguments$height
if(is.null(arguments$pty)) pty='m' else pty=arguments$pty
if(is.null(arguments$res)) res=320 else res=arguments$res

# Graphical arguments
if(is.null(arguments$vsize)) vsize=max((-1/72)*(nNodes)+5.35,1) else vsize=arguments$vsize
if(is.null(arguments$esize)) esize=max((-1/72)*(nNodes)+5.35,1)  else esize=arguments$esize
if(is.null(arguments$color)) color=NULL else color=arguments$color
if(is.null(arguments$bg)) bg=F else bg=arguments$bg
if(is.null(arguments$bgcontrol)) bgcontrol=6 else bgcontrol=arguments$bgcontrol
if(is.null(arguments$bgres)) bgres=100 else bgres=arguments$bgres
if(is.null(arguments$transparency)) transparency=F else transparency=arguments$transparency
if(is.null(arguments$lcolor)) lcolor="black" else lcolor=arguments$lcolor
if(is.null(arguments$loop)) loop=1 else loop=arguments$loop
if(is.null(arguments$legend.cex)) legend.cex=1 else legend.cex=arguments$legend.cex
if(is.null(arguments$borders)) borders=TRUE else borders=arguments$borders
if(is.null(arguments$border.colors)) border.colors=NULL else border.colors=arguments$border.colors
if(is.null(arguments$shape)) shape="circle" else shape=arguments$shape
if(is.null(arguments$label.scale)) label.scale=TRUE else label.scale=arguments$label.scale
if(is.null(arguments$scores)) scores=NULL else scores=arguments$scores
if(is.null(arguments$scores.range)) scores.range=NULL else scores.range=arguments$scores.range
if(is.null(arguments$lty)) lty=NULL else lty=arguments$lty

# Arguments for directed graphs:
if(is.null(arguments$curve)) curve=NULL else curve=arguments$curve
if(is.null(arguments$arrows)) arrows=TRUE else arrows=arguments$arrows
if(is.null(arguments$diag)) diag=F else diag=arguments$diag
if(is.null(arguments$bidirectional)) bidirectional=FALSE else bidirectional=arguments$bidirectional

# Arguments for SVG pictures:
#if(is.null(arguments$tooltips)) tooltips=NULL else tooltips=arguments$tooltops
#if(is.null(arguments$SVGtooltips)) SVGtooltips=NULL else SVGtooltips=arguments$SVGtooltips
#if(is.null(arguments$hyperlinks)) hyperlinks=NULL else hyperlinks=arguments$hyperlinks


# Legend setting 1
if (is.null(legend))
{
	if (is.null(groups)) legend=FALSE else legend=TRUE
}
if (legend & filetype!='pdf' & filetype!='eps')
{
	width=width*1.5
}

# Start output:
if (filetype=='R') windows(width=width,height=height)
if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height,width=width, horizontal=FALSE)
if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height,width=width)
if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),unit='in',res=res,height=height,width=width)
if (filetype=='png') png(paste(filename,".png",sep=""),unit='in',res=res,height=height,width=width)
if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),unit='in',res=res,height=height,width=width)
if (filetype=="svg") 
{
	stop("SVG is not supported in this version because the RSVGTipsDevice only
	supports 32bit. Please see https://sites.google.com/site/qgraphproject/ for
	a version that supports this")
}
#if (filetype=="svg")
#{
#	require("RSVGTipsDevice")
#	devSVGTips(paste(filename,".svg",sep=""),width=width,height=height,title=filename)
#}
#if (!filetype%in%c('pdf','png','jpg','jpeg','svg','R','eps','tiff')) warning(paste("File type",filetype,"is not supported")) 

# Rescale dims:
if (pty=='s')
{
	width=height=min(c(width,height))
}

# Legend setting 2
if (legend & filetype!='pdf')
{
	layout(t(c(1,1,2)))
}

# Weighted settings:
if (is.null(weighted))
{
	if (edgelist)
	{
		if (ncol(adj)==2) weighted=FALSE else weighted=TRUE
	}
	if (!edgelist)
	{
		if (length(unique(c(adj)))>2) weighted=TRUE else weighted=FALSE
	}
}		
if (!weighted) cut=0

# par settings:
par(pty=pty)

if (!edgelist)
{
	if (!is.logical(directed)) if (is.null(directed))
	{
		if (!all(adj==t(adj))) directed=TRUE else directed=FALSE
	}
}
  
background="white"
     
if (is.character(bg)) background=bg
    # Partial graph:
if (partial) 
{
	if (edgelist) stop("Concentration graph requires correlation matrix")
	mi=solve(adj)
	for (i in 1:nrow(adj)) 
	{
		for (j in 1:nrow(adj)) 
		{
			adj[i,j]=-1*mi[i,j]/sqrt(mi[i,i]*mi[j,j]) 
		}
	} 
	adj=round(adj,7) 
}

# Diag:
diagCols=FALSE
diagWeights=0
if (is.character(diag)) 
{
	if (diag=="col" & !edgelist)
	{
		diagWeights=diag(adj)
		diagCols=TRUE
	}
}
if (is.numeric(diag))
{
	if (length(diag)==1) diag=rep(diag,nNodes)
	if (length(diag)!=nNodes) stop("Numerical assignment of the 'diag' argument must be if length equal to the number of nodes")
	diagWeights=diag
	diagCols=TRUE
}
if (is.logical(diag)) if (!diag & !edgelist) diag(adj)=0
	
# CREATE EDGELIST:

if (edgelist)
{
	edgelist.from=adj[,1]
	edgelist.to=adj[,2]
	if (ncol(adj)>2) edgelist.weight=adj[,3] else edgelist.weight=rep(1,length(edgelist.from))
} else
{
	edgelist.from=numeric(0)
	edgelist.to=numeric(0)
	edgelist.weight=numeric(0)

	edgelist.from=rep(1:nrow(adj),times=nrow(adj))
	edgelist.to=rep(1:nrow(adj),each=nrow(adj))
	edgelist.weight=c(adj)

	if (!directed)
	{
		edgelst.from=edgelist.from[c(upper.tri(adj),T)]
		edgelst.to=edgelist.to[c(upper.tri(adj),T)]
		edgelst.weight=edgelist.weight[c(upper.tri(adj),T)]
	}

	edgelist.from=edgelist.from[edgelist.weight!=0]
	edgelist.to=edgelist.to[edgelist.weight!=0]
	edgelist.weight=edgelist.weight[edgelist.weight!=0]
}

maximum=max(abs(c(maximum,max(edgelist.weight),cut,diagWeights)))
if (cut==0)
{
	avgW=(abs(edgelist.weight)-minimum)/(maximum-minimum)
} else avgW=(abs(edgelist.weight)-cut)/(maximum-cut)
avgW[avgW<0]=0


edgesort=sort(abs(edgelist.weight),index.return=T)$ix
edge.width=rep(1,length(edgelist.weight))


# lty and curve settings:
if (is.null(lty))
{
	lty=rep(1,length(edgelist.from))
} else 
{
	if (length(lty)==1) lty=rep(lty,length(edgelist.from))
}

# Make bidirectional vector:
if (length(bidirectional)==1) bidirectional=rep(bidirectional,length(edgelist.from))
if (length(bidirectional)!=length(edgelist.from)) stop("Bidirectional vector must be of legth 1 or equal to the number of edges")


if (directed & is.null(curve))
{
	curve=rep(0,length(edgelist.from))
	for (i in 1:length(edgelist.from))
	{
		if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]))
		{
			if (!bidirectional[i]) curve[i]=0.2		
		}
	}
}
if (any(bidirectional) & any(curve!=0))
{
	for (i in 1:length(edgelist.from))
	{
		if (bidirectional[i] & any(edgelist.from[i:length(edgelist.from)]==edgelist.to[i] & edgelist.to[i:length(edgelist.from)]==edgelist.from[i]))
		{
			edgelist.weight[i]=0
		}
	}
}	

# Layout settings:
if (is.null(layout)) layout="default"
if (!is.numeric(layout))
{
if (layout=="default" & (directed | !weighted)) layout="spring"
if (layout=="default" | layout=="circular") 
{
	if (is.null(groups))
	{
		layout=matrix(0,nrow=nNodes,ncol=2)
		tl=nNodes+1
		layout[,1]=sin(seq(0,2*pi, length=tl))[-tl]
		layout[,2]=cos(seq(0,2*pi, length=tl))[-tl] 
	} else
	{
		if (is.null(rotation)) rotation=rep(0,length=length(groups))
			
		l1=matrix(0,nrow=length(groups),ncol=2)
		tl=nrow(l1)+1
		l1[,1]=sin(seq(0,2*pi, length=tl))[-tl]
		l1[,2]=cos(seq(0,2*pi, length=tl))[-tl]
		l1=l1*length(groups)*layout.control
			
		layout=matrix(0,nrow=nNodes,ncol=2)
		for (i in 1:length(groups)) 
		{
			tl=length(groups[[i]])+1
			layout[groups[[i]],1]=sin(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,1]
			layout[groups[[i]],2]=cos(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,2] 
		}
	}
} else if (layout=="spring")
{
	layout=qgraph.layout.fruchtermanreingold(cbind(edgelist.from,edgelist.to),abs(edgelist.weight),nNodes,groups=groups,rotation=rotation,layout.control=layout.control,
		niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
			constraints=layout.par$constraints)
}
}

# Layout matrix:
if (is.matrix(layout)) if (ncol(layout)>2)
{
	Lmat=layout
	LmatX=seq(-1,1,length=ncol(Lmat))
	LmatY=seq(1,-1,length=nrow(Lmat))
	layout=matrix(0,nrow=nNodes,ncol=2)
	for (i in 1:nNodes)
	{
		loc=which(Lmat==i,arr.ind=T)
		layout[i,]=c(LmatX[loc[2]],LmatY[loc[1]])
	}
}

# Rescale layout:
l=original.layout=layout
if (rescale) {
l[,1]=(l[,1]-min(l[,1]))/(max(l[,1])-min(l[,1]))*2-1
l[,2]=(l[,2]-min(l[,2]))/(max(l[,2])-min(l[,2]))*2-1 }
layout=l


if (weighted) 
{
	edge.width=avgW*(esize-1)+1
	edge.width[edge.width<1]=1

	#Edge color:
	edge.color="#00000000"
	if (cut==0) 
	{
		col=(abs(edgelist.weight)-minimum)/(maximum-minimum)
	} else 
	{
		col=(abs(edgelist.weight)-minimum)/(cut-minimum)
	}
	col[col>1]=1
	col[col<0]=0

	if (transparency) 
	{
		col=col^(2)
		neg=col2rgb(rgb(0.75,0,0))/255
		pos=col2rgb(rgb(0,0.6,0))/255

		# Set colors for edges over cutoff:
		edge.color[edgelist.weight< -1* minimum] <- rgb(neg[1],neg[2],neg[3],col[edgelist.weight< -1*minimum])
		edge.color[edgelist.weight> minimum] <- rgb(pos[1],pos[2],pos[3],col[edgelist.weight> minimum])
	} else 
	{
		edge.color[edgelist.weight>minimum]=rgb(1-col[edgelist.weight > minimum],1-(col[edgelist.weight > minimum]*0.25),1-col[edgelist.weight > minimum])
		edge.color[edgelist.weight< -1*minimum]=rgb(1-(col[edgelist.weight < (-1)*minimum]*0.25),1-col[edgelist.weight < (-1)*minimum],1-col[edgelist.weight < (-1)*minimum])
	}
	if (cut!=0)
	{
		# Set colors for edges over cutoff:
		edge.color[edgelist.weight<= -1*cut] <- "red"
		edge.color[edgelist.weight>= cut] <- "darkgreen"
	}

} else
{
	if (!is.logical(transparency)) Trans=transparency else Trans=1
	edge.width=rep(esize,length(edgelist.weight))
	edge.color=rep(rgb(0.5,0.5,0.5,Trans),length(edgesort))
}


# Vertex color:
if (is.null(color) & !is.null(groups)) color=rainbow(length(groups))

if (is.null(groups)) groups=list(1:nNodes)
if (is.null(color))	color="white"

vertex.colors=rep("white",nNodes)

if (!is.null(groups)) {
for (i in 1:length(groups)) vertex.colors[groups[[i]]]=color[i] }

if (length(color)==nNodes) vertex.colors=color

if (!is.null(scores))
{
	if (length(scores)!=nNodes)
	{
		warning ("Length of scores is not equal to nuber of items")
	} else
	{
		border.colors=vertex.colors
		if (is.null(scores.range)) scores.range=c(min(scores),max(scores))
		scores[is.na(scores)]=scores.range[1]
		rgbmatrix=1-t(col2rgb(vertex.colors)/255)
		for (i in 1:nNodes) rgbmatrix[i,]=rgbmatrix[i,] * (scores[i]-scores.range[1] ) / (scores.range[2]-scores.range[1] )
		vertex.colors=rgb(1-rgbmatrix)
	}
}

if (diagCols)
{
	if (diagCols & !is.null(scores)) stop("Multiple modes specified for vertex colors (diag and scores)")
	if (diagCols & weighted)
	{
		if (is.null(border.colors) & !all(vertex.colors=="white")) border.colors=vertex.colors
		if (cut==0) 
		{
			colV=(abs(diagWeights)-minimum)/(maximum-minimum)
		} else 
		{
			colV=(abs(diagWeights)-minimum)/(cut-minimum)
		}
		colV[colV>1]=1
		colV[colV<0]=0

		if (transparency) 
		{
			vertex.colors=rep("#00000000",nNodes)
			colV=colV^(2)
			neg=col2rgb(rgb(0.75,0,0))/255
			pos=col2rgb(rgb(0,0.6,0))/255

			# Set colors for edges over cutoff:
			vertex.colors[diagWeights< -1* minimum] <- rgb(neg[1],neg[2],neg[3],colV[diagWeights< -1*minimum])
			vertex.colors[diagWeights> minimum] <- rgb(pos[1],pos[2],pos[3],colV[diagWeights> minimum])
		} else 
		{
			vertex.colors=rep("white",nNodes)
			vertex.colors[diagWeights>minimum]=rgb(1-colV[diagWeights > minimum],1-(colV[diagWeights> minimum]*0.25),1-colV[diagWeights > minimum])
			vertex.colors[diagWeights< -1*minimum]=rgb(1-(colV[diagWeights< (-1)*minimum]*0.25),1-colV[diagWeights < (-1)*minimum],1-colV[diagWeights < (-1)*minimum])
		}
		if (cut!=0)
		{
			# Set colors for edges over cutoff:
			vertex.colors[diagWeights<= -1*cut] <- "red"
			vertex.colors[diagWeights>= cut] <- "darkgreen"
		}
	}
}
if (is.null(border.colors))
{
	border.colors=rep("black",length(vertex.colors))
}

# Vertex size:
if (length(vsize)==1) vsize=rep(vsize,nNodes)
if (!edgelist) Vsums=rowSums(abs(adj))+colSums(abs(adj))
if (edgelist)
{
	Vsums=numeric(0)
	for (i in 1:nNodes) Vsums[i]=sum(c(adj[,1:2])==i)
}
if (length(vsize)==2 & nNodes>2 & length(unique(Vsums))>1) vsize=vsize[1] + (vsize[2]-vsize[1]) * (Vsums-min(Vsums))/(max(Vsums)-min(Vsums))
if (length(vsize)==2 & nNodes>2 & length(unique(Vsums))==1) vsize=rep(mean(vsize),nNodes)


# Vertex shapes:
if (length(shape)==1) shape=rep(shape,nNodes)

pch1=numeric(0)
pch2=numeric(0)

for (i in 1:length(shape))
{
	if (shape[i]=="circle")
	{
		pch1[i]=16
		pch2[i]=1
	}
	if (shape[i]=="square")
	{
		pch1[i]=15
		pch2[i]=0
	}
	if (shape[i]=="triangle")
	{
		pch1[i]=17
		pch2[i]=2
	}
	if (shape[i]=="diamond")
	{
		pch1[i]=18
		pch2[i]=5
	}
	if (!shape[i]%in%c("circle","square","triangle","diamond")) stop(paste("Shape",shape[i],"is not supported"))
}


# Super cool background:

if (is.logical(bg)) if (bg) {

colarray=array(dim=c(bgres,bgres,length(groups)))

seq=seq(-1.2,1.2,length=bgres+1)

for (G in 1:length(groups)) {

Xg=l[groups[[G]],1]
Yg=l[groups[[G]],2]

for (i in 1:bgres) {
for (j in 1:bgres) {

Xp=mean(seq[i:(i+1)])
Yp=mean(seq[j:(j+1)])

colarray[i,j,G]=min(sqrt( (Xp-Xg)^2 + (Yp-Yg)^2)) }}}

colarray=((2.2-colarray)/2.2)^bgcontrol

colarray2=array(dim=c(3,bgres,bgres))
 }

 


# PLOT:
par(mar=c(0,0,0,0), bg=background)
if (plot)
{
	plot(1, ann = FALSE, axes = FALSE, xlim = c(-1.2, 1.2), ylim = c(-1.2 ,1.2 ),type = "n", xaxs = "i", yaxs = "i")
}

if (is.logical(bg)) if (bg){


for (i in 1:bgres) {
for (j in 1:bgres) {

for (C in 1:3) {

colarray2[C,i,j]=min(c(1,max(colarray[i,j,]*(col2rgb(color)[C,]/255))))  }

polygon(c(seq[i],seq[i+1],seq[i+1],seq[i]),c(seq[j],seq[j],seq[j+1],seq[j+1]),
	col=rgb(colarray2[1,i,j],colarray2[2,i,j],colarray2[3,i,j]),border=NA)

} }

}     

# Edge labels:
# Make labels:
if (is.logical(edge.labels))
{
	if (edge.labels)
	{
		edge.labels=round(edgelist.weight,2)
	}
}



if (!is.logical(edge.labels))
{
	edge.labels=as.character(edge.labels)
	if (length(edge.labels)!=length(edgelist.from))
	{
		warning("Number of edge labels did not correspond to number of edges, edge labes have been ommited")
		edge.labels=NULL
	}
	midX=numeric(0)
	midY=numeric(0)

	## Set fonts (symbol):
	strsplE=strsplit(edge.labels,"")
	greekE=logical(0)

	for (i in 1:length(strsplE)) 
	{
		greekE[i]=any(strsplE[[i]]=="*")
		edge.labels[i]=paste(strsplE[[i]][which(strsplE[[i]]!="*")],collapse="") 
	}
	edge.font=rep(1,length(edgelist.from))
	edge.font[greekE]=5
}
			
			
			
# Plot edges: 
if (!directed)
{ 
	for (i in edgesort) if (abs(edgelist.weight[i])>minimum) 
	{
		points(layout[c(edgelist.from[i],edgelist.to[i]),1],layout[c(edgelist.from[i],edgelist.to[i]),2],lwd=edge.width[i],col=edge.color[i],type='l',lty=lty[i])
		if (!is.logical(edge.labels))
		{
			midX[i]=mean(layout[c(edgelist.from[i],edgelist.to[i]),1])
			midY[i]=mean(layout[c(edgelist.from[i],edgelist.to[i]),2])
		}
	}	
} else { # Directed edges: 
	if (length(curve)==1) curve=rep(curve,length(edgesort))
	curve[edgelist.from==edgelist.to]=1
	for (i in edgesort)
	{
		if (abs(edgelist.weight[i])>minimum)
		{
			x1=layout[edgelist.from[i],1]
			x2=layout[edgelist.to[i],1]
			y1=layout[edgelist.from[i],2]
			y2=layout[edgelist.to[i],2]
			
			if (!is.logical(edge.labels))
			{
				midX[i]=mean(c(x1,x2))
				midY[i]=mean(c(y1,y2))
			}
			
			if (curve[i]==0)
			{
				if (is.logical(arrows)) if (arrows & directed)
				{
					xd=x2-x1
					yd=y2-y1
					d2=sqrt(sum(xd^2+yd^2))
					if (shape[edgelist.to[i]]!="square")
					{
						x2=x2-xd*(0.5*vsize[edgelist.to[i]]*0.125*(7/width)*par("cin")[2]/d2)
						y2=y2-yd*(0.5*vsize[edgelist.to[i]]*0.125*(7/height)*par("cin")[2]/d2)
					}
					if (shape[edgelist.to[i]]=="square")
					{
						x2=x2-xd*(0.5*vsize[edgelist.to[i]]*0.125*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
						y2=y2-yd*(0.5*vsize[edgelist.to[i]]*0.125*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
					}
					if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]) & bidirectional[i])
					{
						xd=x2-x1
						yd=y2-y1
						d2=sqrt(sum(xd^2+yd^2))
						if (shape[edgelist.to[i]]!="square")
						{
							x1=x1+xd*(0.5*vsize[edgelist.from[i]]*0.125*(7/width)*par("cin")[2]/d2)
							y1=y1+yd*(0.5*vsize[edgelist.from[i]]*0.125*(7/height)*par("cin")[2]/d2)
						}
						if (shape[edgelist.to[i]]=="square")
						{
							x1=x1+xd*(0.5*vsize[edgelist.from[i]]*0.125*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
							y1=y1+yd*(0.5*vsize[edgelist.from[i]]*0.125*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
						}
					}
				}
				lines(c(x1,x2),c(y1,y2),lwd=edge.width[i],col=edge.color[i],lty=lty[i])
				if (!is.logical(arrows))
				{
					Ax=seq(x1,x2,length=arrows+2)
					Ay=seq(y1,y2,length=arrows+2)
					
					arrows(x1,y1,Ax,Ay,lwd=max(edge.width[i]/2,1),col=edge.color[i],
						length=max(0.25*avgW[i],0.1),lty=lty[i])
				}
				else if (arrows)
				{
					arrows(x1,y1,x2,y2,lwd=max(edge.width[i]/2,1),col=edge.color[i],
						length=max(0.25*avgW[i],0.1),lty=lty[i])
					if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]) & bidirectional[i])
					{
						arrows(x2,y2,x1,y1,lwd=max(edge.width[i]/2,1),col=edge.color[i],
						length=max(0.25*avgW[i],0.1),lty=lty[i])
					}
				}
			} else {
				if (edgelist.from[i]==edgelist.to[i])
				{
					loopX=loop*3*(0.5*vsize[edgelist.to[i]]*0.125*(7/width)*par("cin")[2])
					spx=c(x1+loopX,x1,x1-loopX)
					loopY=loop*3*(0.5*vsize[edgelist.to[i]]*0.125*(7/height)*par("cin")[2])
					spy=c(y1,y1+loopY,y1)
					spl=xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=F)
				} else 
				{
					midx <- (x1 + x2)/2
					midy <- (y1 + y2)/2
					spx <- midx - curve[i] * 1/2 * (y2 - y1)
					spy <- midy + curve[i] * 1/2 * (x2 - x1)
					spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=F)
				}	
				if (is.logical(arrows)) if (arrows & directed)
				{
					xd=x2-spl$x[length(spl$x)-1]
					yd=y2-spl$y[length(spl$y)-1]
					d2=sqrt(sum(xd^2+yd^2))
					if (shape[edgelist.to[i]]!="square")
					{
						x2=x2-xd*(0.5*vsize[edgelist.to[i]]*0.125*(7/width)*par("cin")[2]/d2)
						y2=y2-yd*(0.5*vsize[edgelist.to[i]]*0.125*(7/height)*par("cin")[2]/d2)
					}
					if (shape[edgelist.to[i]]=="square")
					{
						x2=x2-xd*(0.5*vsize[edgelist.to[i]]*0.125*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
						y2=y2-yd*(0.5*vsize[edgelist.to[i]]*0.125*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
					}
					if (edgelist.from[i]==edgelist.to[i])
					{
						spx=c(x1+loop,x1,x1-loop)
						spy=c(y1,y1+loop,y1)
						spl=xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=F)
					} else 
					{
						midx <- (x1 + x2)/2
						midy <- (y1 + y2)/2
						spx <- midx - curve[i] * 1/2 * (y2 - y1)
						spy <- midy + curve[i] * 1/2 * (x2 - x1)
						spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=F)
					}
					if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]) & bidirectional[i])
					{
						xd=x2-spl$x[1]
						yd=y2-spl$y[1]
						d2=sqrt(sum(xd^2+yd^2))
						if (shape[edgelist.to[i]]!="square")
						{
							x1=x1+xd*(0.5*vsize[edgelist.from[i]]*0.125*(7/width)*par("cin")[2]/d2)
							y1=y1+yd*(0.5*vsize[edgelist.from[i]]*0.125*(7/height)*par("cin")[2]/d2)
						}
						if (shape[edgelist.to[i]]=="square")
						{
							x1=x1+xd*(0.5*vsize[edgelist.from[i]]*0.125*(7/width)*par("cin")[2]/max(abs(c(xd,yd))))
							y1=y1+yd*(0.5*vsize[edgelist.from[i]]*0.125*(7/height)*par("cin")[2]/max(abs(c(xd,yd))))
						}
						if (edgelist.from[i]==edgelist.to[i])
						{
							spx=c(x1+loop,x1,x1-loop)
							spy=c(y1,y1+loop,y1)
							spl=xspline(c(x1,spx,x2),c(y1,spy,y2),1,draw=F)
						} else 
						{
							midx <- (x1 + x2)/2
							midy <- (y1 + y2)/2
							spx <- midx - curve[i] * 1/2 * (y2 - y1)
							spy <- midy + curve[i] * 1/2 * (x2 - x1)
							spl=xspline(c(x1,spx,x2),c(y1,spy,y2),-1,draw=F)
						}
					}
					
				}
				lines(spl,lwd=edge.width[i],col=edge.color[i],lty=lty[i])
				if (!is.logical(edge.labels))
				{
					midX[i]=spl$x[length(spl$x)/2]
					midY[i]=spl$y[length(spl$y)/2]
				}
				if (directed)
				{
					if (!is.logical(arrows))
					{
						Ax=seq(1,length(spl$x),length=arrows+2)
						Ay=seq(1,length(spl$y),length=arrows+2)
						
						arrows(spl$x[Ax[2:(arrows+1)]],spl$y[Ay[2:(arrows+1)]],spl$x[Ax[2:(arrows+1)]+1],spl$y[Ay[2:(arrows+1)]+1],lwd=max(edge.width[i]/2,1),col=edge.color[i],
							length=max(0.25*avgW[i],0.1),lty=lty[i])
					}
					else if (arrows)
					{
						arrows(spl$x[length(spl$x)-3],spl$y[length(spl$y)-3],x2,y2,lwd=max(edge.width[i]/2,1),col=edge.color[i],
							length=max(0.25*avgW[i],0.1),lty=lty[i])
						if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]) & bidirectional[i])
						{
							arrows(spl$x[3],spl$y[3],x1,y1,lwd=max(edge.width[i]/2,1),col=edge.color[i],
							length=max(0.25*avgW[i],0.1),lty=lty[i])
						}
					}
					
				}
			}
		} 
	}
}

# Edge labels
if (!is.logical(edge.labels))
{
	text(midX,midY,edge.labels,font=edge.font,cex=edge.label.cex)
}			

# Plot nodes:
points(layout,cex=vsize,col=vertex.colors,pch=pch1)
if (borders) points(layout,cex=vsize,lwd=2,pch=pch2,col=border.colors)

	
# Make labels:
if (is.logical(labels))
{
	if (labels)
	{
		labels=1:nNodes
	}
}

if (!is.logical(labels))
{
	labels=as.character(labels)
	# Vertex label symbols:
	strsplV=strsplit(labels,"")
	greekV=logical(0)
	for (i in 1:length(strsplV)) 
	{
		greekV[i]=any(strsplV[[i]]=="*")
		labels[i]=paste(strsplV[[i]][which(strsplV[[i]]!="*")],collapse="") 
	}
	V.font=rep(1,length(edgelist.from))
	V.font[greekV]=5

	label.cex=vsize
	if (label.scale) label.cex[nchar(labels)>1]=label.cex[nchar(labels)>1]*2/nchar(labels[nchar(labels)>1],"width")
	for (i in 1:nNodes) 
	{
		#if (!is.null(tooltips)) if (!is.na(tooltips[i]))
		#{
		#	if (filetype!='svg') warning("Tooltips only supported in SVG filetype.") else setSVGShapeToolTip(desc=tooltips[i])
		#}
		#if (!is.null(SVGtooltips)) if (!is.na(SVGtooltips[i]))
		#{
		#	setSVGShapeToolTip(desc=SVGtooltips[i])
		#}
	text(layout[i,1],layout[i,2],labels[i],cex=label.cex[i]/4,col=lcolor,font=V.font[i])
	}
}

# Plot Legend:
if (legend)
{
	if (is.null(scores))
	{
		legend.cex=legend.cex*2
		plot(1, ann = FALSE, axes = FALSE, xlim = c(-1, 1), ylim = c(-1 ,1 ),type = "n", xaxs = "i", yaxs = "i")

		legend (0,0, names(groups), col= color ,pch = 19, xjust=0.5, yjust=0.5, cex=legend.cex, bty='n')
		legend (0,0, names(groups), col= "black" ,pch = 1, xjust=0.5, ,yjust=0.5, cex=legend.cex, bty='n') 
	}
	if (!is.null(scores))
	{
	plot(1, ann = FALSE, axes = FALSE, xlim = c(0.5, scores.range[2]-scores.range[1]+8), ylim = c(0.5, length(groups)+2),
     type = "n", xaxs = "i", yaxs = "i")
	
	for (i in 1:length(groups)) {

	groupcols="white"
	groupcolors=1-t(col2rgb(color[i])/255)
	c=1
	for (j in (scores.range[1]:scores.range[2]-scores.range[1])/(scores.range[2]-scores.range[1])) 
	{
		groupcols[c]=rgb(1-j*groupcolors)
		c=c+1
	}

	for (j in scores.range[1]:scores.range[2]-scores.range[1]) {
	
		polygon(c(j,j,j+1,j+1),c(i+0.05,i+0.95,i+0.95,i+0.05),col=groupcols[j],border=border.colors[i],lwd=2)
		
		} 
	text(j+1.5,i+0.5,names(groups)[i],pos=4)
		}

for (i in scores.range[1]:scores.range[2]-scores.range[1]) text(i+1.5,length(groups)+1.5,i/2)

	
	}
}

# Plot details:
if (details & weighted)
{
	if (cut != 0) text(0,-1.15,paste("Cutoff:",round(cut,2)),cex=0.6)
	if (minimum != 0) text(-1.2,-1.15,paste("Minimum:",round(minimum,2)),pos=4,cex=0.6)
	text(1.2,-1.15,paste("Maximum:",round(maximum,2)),pos=2,cex=0.6)
}

	
if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff')) 
{
	print(paste("Output stored in ",getwd(),"/",filename,".",filetype,sep=""))
	dev.off()
}

# Make output list:
#returnval=list(adj=adj, layout=layout, cut=cut, maximum=maximum, minimum=minimum, groups=groups, weighted=weighted, rescale=rescale, labels=labels, directed=directed, legend=legend, plot=plot, rotation=rotation, layout.control=layout.control, layout.par=layout.par, filetype=filetype, filename=filename, width=width, height=height, pty=pty, res=res, vsize=vsize, esize=esize, color=color, bg=bg, bgcontrol=bgcontrol, bgres=bgres, transparency=transparency, lcolor=lcolor, loop=loop, legend.cex=legend.cex, borders=borders, curve=curve, arrows=arrows, diag=diag, tooltips=tooltips, hyperlinks=hyperlinks)

returnval=arguments
returnval$layout=layout

class(returnval)="qgraph"

invisible(returnval)
}
 
