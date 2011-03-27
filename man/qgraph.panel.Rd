\name{qgraph.panel}
\alias{qgraph.panel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qgraph.panel
}
\description{
Creates a 4-panel graph. See details. The usage is the same as \code{\link{qgraph}}
}
\usage{
qgraph.panel(adj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{adj}{The adjacency matrix or edgelist. See \code{\link{qgraph}}. Correlation matrix is recommended.}
  \item{\dots}{Optional additional arguments (only 'layout' and 'graph' are omitted). See \code{\link{qgraph}}.}
}
\details{
This function will create a 4-panel plot containing four plots useful in analyzing correlation matrices:
\enumerate{
\item Association graph with circular layout
\item Association graph with spring layout
\item Concentration graph with spring layout
\item Factorial graph with spring layout
}
}
\author{
Sacha Epskamp
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.svg}}
}
\examples{
data(big5)
data(big5groups)

qgraph.panel(cor(big5),groups=big5groups,minimum=0.2,borders=FALSE,vsize=1,cut=0.3)
}
