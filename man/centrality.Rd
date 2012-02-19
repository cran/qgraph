\encoding{UTF8}
\name{centrality}
\alias{centrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Centrality statistics of graphs
}
\description{
This function can be used on the output of \code{\link{qgraph}} to compute the node centrality statistics for weighted graphs proposed by Opsahl, Agneessens and Skvoretz (2010).
}
\usage{
centrality(graph, alpha = 1, posfun = abs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
A "qgraph" object obtained from \code{\link{qgraph}}}
  \item{alpha}{
The tuning parameter. Defaults to 1. 
}
  \item{posfun}{
A function that converts positive and negative values to only positive. Defaults to the absolute value.
}
}
\details{
This function computes and returns the in and out degrees, closeness and betweenness as well as the shortest path lengths and shortest paths between all pairs of nodes in the graph. For more information on these statistics, see Opsahl, Agneessens and Skvoretz (2010).

These statistics are only defined for positive edge weights, and thus negative edge weights need to be transformed into positive edge weights. By default, this is done by taking the absolute value.

The algorithm used for computing the shortest paths is the well known "Dijkstra’s algorithm" (Dijkstra, 1959). The algorithm has been implemented in R, which can make this function take several minutes to run for large graphs (over 100 nodes). A future version of qgraph will include a compiled version to greatly speed up this function.
}
\value{
A list containing:
\item{OutDegree }{A vector containing the outward degree of each node.}
\item{InDegree }{A vector containing the inward degree of each node.}
\item{Closeness }{A vector containing the closeness of each node.}
\item{Betweenness }{A vector containing the betweenness of each node}
\item{ShortestPathLengths }{A matrix containing the shortest path lengths of each pairs of nodes. These path lenghts are based on the inverse of the edge weights raised to the power lambda.}
\item{ShortestPaths }{A matrix of lists containing all shortest path lengths between all pairs of nodes. Use double square brackets to index. E.g., if the list is called 'res', res$ShortestPaths[[i,j]] gives a list containing all shortest paths between node i and j.}
}
\references{
Opsahl, T., Agneessens, F., Skvoretz, J. (2010). Node centrality in weighted networks: generalizing degree and shortest paths. Soc Netw. 32:245–251.

Dijkstra, E.W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik 1, 269–271.

}
\author{
Sacha Epskamp (qgraph@sachaepskamp.com)
}
\seealso{
\code{\link{qgraph}}
}
\examples{
set.seed(1)
adj <- matrix(sample(0:1,10^2,TRUE,prob=c(0.8,0.2)),nrow=10,ncol=10)
Q <- qgraph(adj)

centrality(Q)
}

\keyword{ graphs }
