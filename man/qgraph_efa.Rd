\name{qgraph.efa}
\alias{qgraph.efa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.efa}
\description{
This function performs an Exploratory Factor Analysis (EFA) using the \code{\link{factanal}} (stats) function and sends the acquired factor loadings to \code{\link{qgraph.loadings}}.
}
\usage{
qgraph.efa( cor, factors=1, rotation="promax", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{cor}{A correlation matrix}
	\item{factors}{The number of factors to extract}
	\item{rotation}{rotation to be used. Can be "varimax", "promax" or "none"}
	\item{...}{arguments passed to \code{\link{qgraph.loadings}}}
}

\references{
https://sites.google.com/site/qgraphproject
}
\author{
Sacha Epskamp (s.epskamp@uva.nl)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.loadings}}
}
\examples{
\dontrun{
#### VISUALIZE CORRELATION MATRIX ###
eta=matrix(rnorm(200*5),ncol=5)
lam=matrix(0,nrow=100,ncol=5)
for (i in 1:5) lam[(20*i-19):(20*i),i]=rnorm(20,0.7,0.3)
eps=matrix(rnorm(200*100),ncol=100)
Y=eta\%*\%t(lam)+eps

# Run qgraph:
qgraph.efa(cor(Y),5)

# Show crossloadings:
qgraph.efa(cor(Y),5,crossloadings=T,cut=0)
}
}
