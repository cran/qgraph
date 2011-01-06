# Main qgraph function

qgraph.layout=function( adj, ... )
{


arguments=list(...)

if (length(arguments)>0)
{
for (i in 1:length(arguments))
{
	if (class(arguments[[i]])=="qgraph") 
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

# Default for fact cut and groups:
if(is.null(arguments$graph)) graph="association" else graph=arguments$graph
if (graph=="factorial") fact=TRUE else fact=FALSE
if (graph=="concentration") partial=TRUE else partial=FALSE
if(is.null(arguments$cut)) 
{
	if (nrow(adj)<50) cut=0 
	if (nrow(adj)>=50 | fact) cut=0.3
} else cut=arguments$cut

if(is.null(arguments$groups)) groups=NULL else groups=arguments$groups

# Factorial graph:
if(is.null(arguments$nfact))
{
 if (is.null(groups)) nfact=round(nrow(adj)/2,0) else nfact=length(groups)
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
if (nrow(adj)<50)  minimum=0
if (nrow(adj)>=50)  minimum=0.1
}	else minimum=arguments$minimum
if(is.null(arguments$weighted)) weighted=NULL else weighted=arguments$weighted
if(is.null(arguments$rescale)) rescale=T else rescale=arguments$rescale
if(is.null(arguments$labels)) labels=TRUE else labels=arguments$labels
if(is.null(arguments$directed)) directed=NULL else directed=arguments$directed
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
if(is.null(arguments$vsize)) vsize=max((-1/72)*(nrow(adj))+5.35,1) else vsize=arguments$vsize
if(is.null(arguments$esize)) esize=max((-1/72)*(nrow(adj))+5.35,1)  else esize=arguments$esize
if(is.null(arguments$color)) color=NULL else color=arguments$color
if(is.null(arguments$bg)) bg=F else bg=arguments$bg
if(is.null(arguments$bgcontrol)) bgcontrol=6 else bgcontrol=arguments$bgcontrol
if(is.null(arguments$bgres)) bgres=100 else bgres=arguments$bgres
if(is.null(arguments$transparency)) transparency=F else transparency=arguments$transparency
if(is.null(arguments$lcolor)) lcolor="black" else lcolor=arguments$lcolor
if(is.null(arguments$loop)) loop=0.15 else loop=arguments$loop
if(is.null(arguments$legend.cex)) legend.cex=1 else legend.cex=arguments$legend.cex
if(is.null(arguments$borders)) borders=TRUE else borders=arguments$borders
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
#if(is.null(arguments$tooltips)) SVGtooltips=NULL else SVGtooltips=arguments$SVGtooltops
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

#Rescale dims:
if (pty=='s')
{
	width=height=min(c(width,height))
}


# Weighted settings:
if (is.null(weighted))
{
	if (length(unique(c(adj)))>2) weighted=TRUE else weighted=FALSE
}		
if (!weighted) cut=0


if (!is.logical(directed)) if (is.null(directed))
{
	if (!all(adj==t(adj))) directed=TRUE else directed=FALSE
}
  
background="white"
     
if (is.character(bg)) background=bg
    # Partial graph:
if (partial) 
{
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
if (!diag) diag(adj)=0
	
# CREATE EDGELIST:

edgelist.from=numeric(0)
edgelist.to=numeric(0)
edgelist.weight=numeric(0)

#if (directed)
#{
	edgelist.from=rep(1:nrow(adj),times=nrow(adj))
	edgelist.to=rep(1:nrow(adj),each=nrow(adj))
	edgelist.weight=c(adj)

#} else {	
#for (i in 1:(nrow(adj)-1)) {
#
#	edgelist.from=c(edgelist.from,rep(i,ncol(adj)-i))
#	edgelist.to=c(edgelist.to,(i+1):ncol(adj))
#	edgelist.weight=c(edgelist.weight,adj[i,(i+1):ncol(adj)]) }
#}	

if (!directed)
{
	edgelst.from=edgelist.from[c(upper.tri(adj),T)]
	edgelst.to=edgelist.to[c(upper.tri(adj),T)]
	edgelst.weight=edgelist.weight[c(upper.tri(adj),T)]
}

edgelist.from=edgelist.from[edgelist.weight!=0]
edgelist.to=edgelist.to[edgelist.weight!=0]
edgelist.weight=edgelist.weight[edgelist.weight!=0]

maximum=max(c(maximum,max(edgelist.weight),cut))
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
	ltyset=T
	lty=rep(1,length(edgelist.from))
} else 
{
	ltyset=F
	lty=rep(1,length(edgelist.from))
}

if (directed & is.null(curve))
{
	curve=rep(0,length(edgelist.from))
	for (i in 1:length(edgelist.from))
	{
		if (any(edgelist.from==edgelist.to[i] & edgelist.to==edgelist.from[i]))
		{
			if (!bidirectional) curve[i]=0.2
			if (bidirectional & any(edgelist.from[i:length(edgelist.from)]==edgelist.to[i] & edgelist.to[i:length(edgelist.from)]==edgelist.from[i]))
			{
				edgelist.weight[i]=0
			}			
		}
	}
}


# Layout settings:
if (is.null(layout)) layout="default"
if (!is.numeric(layout))
{
if (layout=="default" & (directed | !weighted)) layout="spring"
if (layout=="default" | layout=="circulair") 
{
	if (is.null(groups))
	{
		layout=matrix(0,nrow=nrow(adj),ncol=2)
		tl=nrow(adj)+1
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
			
		layout=matrix(0,nrow=nrow(adj),ncol=2)
		for (i in 1:length(groups)) 
		{
			tl=length(groups[[i]])+1
			layout[groups[[i]],1]=sin(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,1]
			layout[groups[[i]],2]=cos(seq(rotation[i],rotation[i]+2*pi, length=tl))[-tl]+l1[i,2] 
		}
	}
} else if (layout=="spring")
{
	layout=qgraph.layout.fruchtermanreingold(cbind(edgelist.from,edgelist.to),abs(edgelist.weight),nrow(adj),groups=groups,rotation=rotation,layout.control=layout.control,
		niter=layout.par$niter,max.delta=layout.par$max.delta,area=layout.par$area,cool.exp=layout.par$cool.exp,repulse.rad=layout.par$repulse.rad,init=layout.par$init,
			constraints=layout.par$constraints)
}
}

# Rescale layout:
l=original.layout=layout
if (rescale) {
l[,1]=(l[,1]-min(l[,1]))/(max(l[,1])-min(l[,1]))*2-1
l[,2]=(l[,2]-min(l[,2]))/(max(l[,2])-min(l[,2]))*2-1 }
layout=l

return(layout)
}
 
