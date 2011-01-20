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
\dontrun{
#### VISUALIZE CORRELATION MATRIX ###
eta=matrix(rnorm(200*5),ncol=5)
lam=matrix(0,nrow=100,ncol=5)
for (i in 1:5) lam[(20*i-19):(20*i),i]=rnorm(20,0.7,0.3)
eps=matrix(rnorm(200*100),ncol=100)
Y=eta\%*\%t(lam)+eps

## run qgraph:
qgraph.panel(cor(Y),groups=list(1:20,21:40,41:60,61:80,81:100),cut=0.4,vsize=3)
}
}
