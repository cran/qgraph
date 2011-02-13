\encoding{UTF8}
\name{qgraph}
\alias{qgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph}
\description{
This is the main function of qgraph. It can create several types of networks (undirected/directed and unweighted/weighted), but is typically used to visualize a correlation matrix.}
\usage{
qgraph( adj, ... )
}
\arguments{
	\item{adj}{Can be either an adjacency matrix (recommended) or an edgelist. An adjacency matrix is a square matrix in which each row and column represents a node and the element between each row and column the edge between those two nodes. Typically this is a correlation matrix, but any type of adjacency matrix can be used. If this matrix only contains binary values, an unweighted graph is created. If this matrix is not symmetrical, a directional graph is created. The edgelist i a matrix with a row for each edge containing the origin in the first column the destination in the second column and the weight (optional) in the third column. Origin and destination node must be coded by a number between 1 and the total number of nodes.}
	\item{...}{Any additional arguments described below. Also a list with class "qgraph" can be added that contains any of these arguments (this is returned by the function)}
}	
\section{Additional optional arguments}{	
\describe{
	\item{layout}{This argument controls the layout of the graph. "circle" gives a circular layout, "groups" gives a layout in which each group is put in a circle and "spring" gives a force embedded layout. It also can be a matrix with a row for each node and x and y coordinates in the first and second column respectively. Defaults to "circle" in weighted graphs without a groups list, "groups" in weighted graphs with a groups list, and "spring" in unweighted graphs.}
	\item{details}{Logical indicating if minimum, maximum and cutoff score should be printed under the graph. Defaults to FALSE.}
	\item{graph}{Type of graph to be made, for use with a correlation matrix as input. "association" will plot the matrix as is, "concentration" will first compute partial correlations between each pair of nodes (controlled for all other variables) and "factorial" will create a graph based on an exploratory factor analysis. See details.}
	\item{cut}{In weighted graphs, this argument can be used to make edges with absolute weights over this value of the strongest color intensity and become wider the stronger they are, and edges with absolute weights under this value of the smallest width and become vaguer the smaller the weight. If this is set to NULL, no cutoff is used and all edges vary in width and color. Defaults to NULL for graphs with less then 50 nodes and 0.3 to larger graphs.}
	\item{groups}{A list object with the measurement model. Each element represents one factor, and must contain the numbers of the nodes in that factor (same order as in the adjacency matrix). This can be used to group the nodes and give them colors.}
	\item{color}{A vector with a color for each element in the groups list, or a color for each node. Defaults to "white" without groups list and rainbow(length(groups)) with a groups list.}
	\item{legend}{Logical value indicating if a legend should be plotted}
	\item{minimum}{Edges with absolute weights under this value are omitted. Defaults to 0 for graphs with less than 50 nodes or 0.1 for larger graphs.}
	\item{maximum}{qgraph regards the highest of the maximum or highest absolute edge weight as the highest weight to scale the edge widths too. To compare several graphs, set this argument to a higher value than any edge weight in the graphs (typically 1 for correlations).}
	\item{vsize}{A value indicating the size of the nodes. Can also be a vector of length 2 (nodes are scaled to degree) or a size for each node.}
	\item{esize}{Size of the largest edge (or what it would be if there was an edge with weight maximum).}
	\item{filetype}{A character containing the file type to save the output in. "R" outputs in a new R window, "pdf" creates a pdf file. "svg" creates a svg file (requires RSVGTipsDevice). 'jpg', 'tiff' and 'png' can also be used. If this is given any other string (e.g. filetype="") no device is opened. Defaults to 'R' if the current devide is the NULL-device or no new device if there already is an open device.}
	\item{filename}{Name of the file, if outputting in pdf}
	\item{width}{Width of the plot, in inches}
	\item{height}{Height of the plot, in inches}
	\item{labels}{If FALSE, no labels are plotted. If TRUE, order in adjacency matrix is used as labels. This can also be a vector with a label for each node. Defaults to TRUE. If a label contains an asterisk (e.g. "x1*") then the asterisk will be omitted and the label will be printed in symbol font (use this for Greek letters)}
	\item{edge.labels}{If FALSE, no edge labels are plotted. If TRUE, numerical edge weights are printed on the edges. This can also be a vector with a label for each edge. Defaults to FALSE. If a label contains an asterisk (e.g. "y1*") then the asterisk will be omitted and the label will be printed in symbol font (use this for Greek letters)}
	\item{edge.label.cex}{Either a single number or a number per edge used as a scalar of the edge label size. Defaults to 1.}
	\item{bg}{If this is TRUE, a background is plotted in which node colors cast a light of that color on a black background. Can also be a character containing the color of the background Defaults to FALSE}
	\item{bgcontrol}{The higher this is, the less light each node gives if bg=TRUE. Defaults to 6.}
	\item{bgres}{square root of the number of pixels used in bg=TRUE, defaults to 100.}
	\item{transparency}{In weighted graphs: logical indicating if the edges should fade to white or become more transparent (use this only if you use a background). In directed graphs this is a value between 0 and 1 indicating the level of transparancy.}
	\item{lcolor}{Character containing the color of the labels, defaults to "black"}
	\item{diag}{Should the diagonal also be plotted as edges? defaults to FALSE. Can also be "col" to plot diagonal values as vertex colors.}
	\item{loop}{If diag=T, this can be used to scale the size of the loop. defaults to 1.}
	\item{legend.cex}{Scalar of the legend. defaults to 1}
	\item{borders}{Logical indicating if borders should be plotted, defaults to TRUE.}
	\item{shape}{A character containing the shape of the nodes. "circle", "square", "triangle" and "diamond" are supported. Can also be a vector with a shape for each node. Defaults to "circle".}
	\item{label.scale}{Logical indicating if labels should be scaled to fit the node. Defaults to TRUE.}
	\item{scores}{This argument can be used to plot scores of an individual on the test. Should be a vector with the scores for each item. Defaults to NULL.}
	\item{scores.range}{Vector of length two indicating the range of the scores, of scores is not NULL.}
	\item{lty}{Line type, see 'par'}
	\item{curve}{A value indicating how strongly edges should be curved. Defaults to 0 (no curve) for each edge except for edges in directional graphs between nodes that have two edges in between them. CURVING LINES DRASTICALLY INCREASES FILE SIZE!}
	\item{arrows}{A logical indicating if arrows should be drawn, or a number indicating how much arrows should be drawn on each edge. If this is TRUE, a simple arrow is plotted, if this is a number, arrows are put in the middle of the edges.}
	\item{asize}{Scalar of the length of arrow heads, in inches. Defaults to 0.15.}
	\item{open}{Logical indicating if open (TRUE) or closed (FALSE) arrowheads should be drawn.}
	\item{bidirectional}{If this is TRUE, Then directional edges between nodes that have two edges between them are not curved.}
	\item{tooltips}{A vector with tooltips for each node, only when filetype='svg'}
	\item{nNodes}{The number of nodes, only needs to be specified if the first argument is an edge-list and some nodes have no edges}
	\item{vTranse}{Transparency of the nodes, must be an integer between 0 and 255, 255 indicating no transparancy. Defaults to 255}
}}

\details{
The qgraph function has a lot of arguments. But most default values work very well, so often only a few arguments are needed. 
This version is not yet completed, so there could be bugs. For any remarks or questions, please contact the author.
}
\section{Creating graphs}{
This function creates a graph based on an adjacency matrix (recommended) or edgelist for the first argument ('adj'). The graph
will be directed or undirected and weighted or unweighted based on the input file (e.g. a symmetric adjacency
matrix with only ones and zeros will result in an undirected unweighted graph) but this can also be specified
with the 'weighted' and 'directed' arguments.

The main idea behind qgraph is to visualize a correlation matrix as a graph. If a correlation matrix is used as
input (recommended) a graph will be created with a node for each variable and edges between those nodes representing
the strength of the correlation between those variables (the correlation becomes the edge weight). Green edges represent positive correlations and red edges
represent negative correlations.

Correlation matrices can be graphed in three ways, indicated by the 'graph' argument. If graph="association" the edge weights correspond
directly to the correlations. If graph="concentration" then the edge weights become the partial correlations between each pair of nodes 
controlled for all other nodes. Finally if graph="factorial" an exploratory factor analysis is performed with \code{\link{factanal}} using
promax rotation and 'nfact' factors (defaults to half of the number of variables). An undirected graph is made in which each variable is connected
to another variable if they both load higher then the value of 'cut' on the same factor.

An edgelist can also be used. This is a matrix (not a list!) with two columns and a row for each edge. The first column indicates the number
of the start of the edge and the second column indicates the number of the end of the edge. Numbering must start with 1. If any
integer is missing between 1 and the highest specified number then that is regarded as a node with no edges (if there is
supposed to be more nodes with no edges after the highest number in the edgelist the 'nNodes' argument can be used).
To create a weighted graph edge weights can be added as a third column in the edgelist.}

\section{Interpreting graphs}{
The width of the edges is specified with the 'esize' arguments and corresponds to the 'lwd' argument in \code{\link{par}}.
Edges with an absolute value under the 'minimum' argument are omitted (useful to keep filesizes from inflating in very large 
graphs). If the 'cut' argument equals 0 (see next paragraph) An (hypothetical) edge with an absolute weight of the minimum value would be white and has width 1, Edges 
with absolute weights higher than the minimum will be wider and more colorful, and the edge with the strongest absolute
weight will be full red (negative) or green (positive) and have the width specified with 'esize'. It is possible to set this strongest edge by using
the 'maximum' argument. When 'maximum' is set to a value above any absolute weight in the graph that value is considered the strongest
edge (this must be done to compare different graphs).

In larger graphs the above edge settings can become very uninterpretable, and setting a cutoff score is advised.
This can be done with the 'cut' argument. With the 'cut' argument a cutoff value can be set which splits scaling
of color and width. Edges with absolute weights under the cutoff score will have width 1 and become more colorful
as they approach the cutoff score, and edges with absolute weights over the cutoff score will be full red or green
and become wider the stronger they are. 

The size of the nodes can be set with the 'vsize' argument. If this has two values then the nodes will be scaled according
to their degree (sum of the row and column of the node).}

\section{Specifying the layout}{
The placement of the nodes (i.e. the layout) is specified with the 'layout' argument. It can be manually specified
by entering a matrix for this argument. The matrix must have a row for each node and two columns indicating its X and
Y coordinate respectively. qgraph plots the nodes on a (-1:1)(-1:1) plane, and the given coordinates will be rescaled
to fit this plane unless 'rescale' is FALSE (not recommended). 

Another option to manually specify the layout is by entering a matrix with more then two columns. This matrix must then
consist of zeroes and a number (the order in the adjacency matrix) for each node indicating it's place. For example:

0 0 2 0 0

1 0 3 0 4

will place node 2 at the top in the center, node 1 at the bottom left corner, node 3 at the bottom in the center and node 4
at the bottom right corner.

It is recommended however that one of the integrated layouts is used. 'layout' can be given a character as argument to accomplish
that. layout="circle" will simply place all nodes in a circle and layout="groups" will place the nodes in separate circles per group
(see next section).

The circle and groups layouts and manual layouts are convenient to see how well the data conforms to a model, but to show how the data
clusters another layout is more appropriate. By specifying layout="spring" the Fruchterman-reingold algorithm (Fruchterman & Reingold, 1991), which has been ported
from the SNA package (Butts, 2010), can be used to create a force-directed layout. In principle, what this function does is that each node
(connected and unconnected) repulse each other, and connected nodes also attract each other. Then after a number of iterations (500 by default)
in which the maximum displacement of each node becomes smaller a layout is achieved in which the distance between nodes correspond very
well to the absolute edge weight between those nodes.

A solution to use this function for weighted graphs has been taken from the igraph package (Csardi G & Nepusz T, 2006) in which the same
function was ported from the SNA package. New in qgraph are the option to include constraints on the nodes by fixing
a coordinate for nodes or reducing the maximum allowed displacement per node. This can be done with the 'layout.par' argument.
For more information see \code{\link{qgraph.layout.fruchtermanreingold}}.

By default, 'layout' is set to "spring" for unweighted and directed graphs, "circle" to weighted undirected graphs (e.g. a correlation matrix)
without 'groups' specified (see next section) and "groups" to weighted undirected graphs when "groups" is specified.}

\section{Specifying a measurement model}{
A measurement model can be specified with the 'groups' argument. This must be a list in which each element is a vector
containing the numbers of nodes that belong together (numbers are taken from the order in the adjacency matrix). All numbers
must be included. If a groups list is specified the "groups" layout can be used to place these nodes together, the nodes
in each group will be given a color, and a legend can be plotted (by setting 'legend' to TRUE). The colors will be taken from the
'color' argument, or be generated with the \code{\link{rainbow}} function.}

\section{Output}{
By default qgraph will plot the graph in a new R window. However the graphs
are optimized to be plotted in a PDF file. To create a pdf file set the 'filetype' argument to "pdf". 'filename' can be used
to specify the filename and folder to output in. 'height' and 'width' can be used to specify the height and width of the image
in inches. By default a new R window is opened if the current device is the NULL-device, otherwise the current device is used (note that when doing this 'width' and 'height' still optimize the image for those
widths and heights, even though the output screen size isn't affected, this is especially important for directed graphs!).

The graphs can also be outputted in an SVG file using the RSVGTipsDevice package (Plate, 2009). An SVG image can be opened in most
browsers (firefox and chrome are recommended), and can be used to display tooltips. Each node can be given a tooltip
with the 'tooltips' argument. The function \code{\link{qgraph.svg}} can be used to make a battery of svg pictures with hyperlinks
to each other, working like a navigation menu.

IMPORTANT NOTE: RSVGTipsDevice is a 32-bit only package, so SVG functionality is not available in 64bit versions of R.}

\section{Additional information}{
By default, edges will be straight between two nodes unless there are two edges between two nodes. To overwrite this the 'bidirectional'
argument can be set to TRUE, which will turn two edges between two nodes into one bidirectional edge. 'bidirectional' can also
be a vector with TRUE or FALSE for each edge.

To specify the strength of the curve the argument 'curve' can be used (but only in directional graphs). 'curve' must be given a numerical value that
represent an offset from the middle of the straight edge through where the curved edge must be drawn. 0 indicates no curve, and any other value 
indicates a curve of that strength. A value of 0.3 is recommended for nice curves. This can be either one number or a vector with the curve of each edge.

Nodes and edges can be given labels with the 'labels' and the 'edge.labels' arguments. 'labels' can be set to FALSE to omit labels, TRUE (default) to set labels
equal to the node number (order in the adjacency matrix) or it can be a vector with the label for each node. Edge labels can also be set to FALSE to
be omitted (default). If 'edge.labels' is TRUE then the weight of each label is printed. Finally, 'edge.labels' can also be a vector with the label for each edge.
If a label (both for edges and nodes) contain an asterisk then the asterisk is omitted and that label is printed in the symbol font (useful to print Greek letters).

A final two things to try: the 'scores' argument can be given a vector with the scores of a person on each variable, which will then be shown using colors of the nodes,
 And the 'bg' argument can be used to change the background of the graph to another color, or use bg=TRUE for a special background (do set transparency=TRUE when using
 background colors other then white).}

\section{Debugging}{
If this function crashes for any reason, run:

dev.off()

To shut down the output device!} 
 
\value{
qgraph returns a 'qgraph' object that contains all the arguments used, with the exeption of the 'layout' argument which 
is set to the final layout used in the graph.
This can then be sent to a new qgraph function to use the same arguments in the new plot.

One of the values returned is 'layout.orig', the original (not rescaled) layout, which needs to be used when using constraint layouts.}


\references{
https://sites.google.com/site/qgraphproject

Carter T. Butts <buttsc@uci.edu> (2010). sna: Tools for Social Network Analysis. R package version 2.2-0. http://CRAN.R-project.org/package=sna

Csardi G, Nepusz T (2006). The igraph software package for complex network research, InterJournal, Complex Systems 1695. http://igraph.sf.net

Plate, T. <tplate@acm.org> and based on RSvgDevice by T Jake Luciani <jakeluciani@yahoo.com> (2009). RSVGTipsDevice: An R SVG graphics device with dynamic tips and hyperlinks. R package version 1.0-1.

Fruchterman, T. & Reingold, E. (1991). Graph drawing by force-directed placement. Software - Pract. Exp. 21, 1129–1164.

}
\author{
Sacha Epskamp<s.epskamp@uva.nl>
}

\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.efa}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.loadings}}
\code{\link{qgraph.sem}}
\code{\link{qgraph.svg}}
\code{\link{qgraph.panel}}
}
\examples{
\dontrun{
#### VISUALIZE CORRELATION MATRIX ###
eta=matrix(rnorm(200*5),ncol=5)
lam=matrix(0,nrow=100,ncol=5)
for (i in 1:5) lam[(20*i-19):(20*i),i]=rnorm(20,0.7,0.3)
eps=matrix(rnorm(200*100),ncol=100)
Y=eta\%*\%t(lam)+eps

## run qgraph:
Q=qgraph(cor(Y),groups=list(1:20,21:40,41:60,61:80,81:100),cut=0.4,vsize=3)

## Same graph but now with spring layout:
qgraph(cor(Y),layout="spring",Q)


#### UNWEIGHTED DIRECTED GRAPHS ###
adj=matrix(sample(0:1,10^2,T,prob=c(0.8,0.2)),nrow=10,ncol=10)
qgraph(adj)

# Save plot to nonsquare pdf file:
qgraph(adj,filetype='pdf',height=5,width=10)


#### EXAMPLES FOR EDGES UNDER DIFFERENT ARGUMENTS ###
# Create edgelist:
E=t(c(1,2))
for (i in 2:11*2) E=rbind(E,(i-1):i)
E=cbind(E,seq(-0.5,0.5,length=11))

# Create layout matrix
L=matrix(1:22,nrow=2)

# Run qgraph with default settings:
Q=qgraph(E,layout=L,labels=FALSE,width=7,height=4,edge.labels=T)

# Default settings with different minimum and maximum:
qgraph(E,Q,maximum=1,minimum=0.1)

# Default settings with cutoff score:
qgraph(E,Q,cut=0.3)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Graphs }
\keyword{ qgraph }
\keyword{ Correlations}
