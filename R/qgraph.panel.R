
qgraph.panel=function(input, ...)
{
Q=list(...)
class(Q)="qgraph"

# Output Q:
if(is.null(Q$filetype)) filetype="default" else filetype=Q$filetype
if(is.null(Q$filename)) filename="qgraph" else filename=Q$filename
if(is.null(Q$width))
{
	if (is.null(dev.list()[dev.cur()])) width=7/2 else width=dev.size(units="in")[1]/2
} else width=Q$width
if(is.null(Q$height))
{
	if (is.null(dev.list()[dev.cur()])) height=7/2 else height=dev.size(units="in")[2]/2
} else height=Q$height
if(is.null(Q$pty)) pty='m' else pty=Q$pty
if(is.null(Q$res)) res=320 else res=Q$res

# Start output:
if (filetype=='default') if (is.null(dev.list()[dev.cur()])) dev.new(rescale="fixed",width=width*2,height=height*2)
if (filetype=='R') dev.new(rescale="fixed",width=width*2,height=height*2)
if (filetype=='eps') postscript(paste(filename,".eps",sep=""),height=height*2,width=width*2, horizontal=FALSE)
if (filetype=='pdf') pdf(paste(filename,".pdf",sep=""),height=height*2,width=width*2)
if (filetype=='tiff') tiff(paste(filename,".tiff",sep=""),unit='in',res=res,height=height*2,width=width*2)
if (filetype=='png') png(paste(filename,".png",sep=""),unit='in',res=res,height=height*2,width=width*2)
if (filetype=='jpg' | filetype=='jpeg') jpeg(paste(filename,".jpg",sep=""),unit='in',res=res,height=height*2,width=width*2)
if (filetype=="svg")
{
	if (R.Version()$arch=="x64") stop("RSVGTipsDevice is not available for 64bit versions of R.")
	require("RSVGTipsDevice")
	devSVGTips(paste(filename,".svg",sep=""),width=width*2,height=height*2,title=filename)
}
if (filetype=="tex") stop("Tex is not yet supported in qgraph.panel")

layout(matrix(1:4,nrow=2,ncol=2))

qgraph(input,filetype="",Q,layout="circular",graph="association",legend=FALSE)
qgraph(input,layout="spring",filetype="",Q,graph="association",legend=FALSE)
qgraph(input,graph="concentration",layout="spring",filetype="",Q,legend=FALSE)
qgraph(input,graph="factorial",layout="spring",filetype="",Q,legend=FALSE,layout.par=list(area=nrow(input)^2.3,repulse.rad=nrow(input)^2.8))

if (filetype%in%c('pdf','png','jpg','jpeg','svg','eps','tiff','tex')) 
{
	print(paste("Output stored in ",getwd(),"/",filename,".",filetype,sep=""))
	dev.off()
}
invisible(Q)
}
