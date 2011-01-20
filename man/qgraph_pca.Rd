\name{qgraph.pca}
\alias{qgraph.pca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.pca}
\description{
This function performs an Principal Component Analysis (PCA) using the 'princomp' function of the psych package (Revelle, 2010)  and sends the acquired factor loadings to \code{\link{qgraph.loadings}}.
}
\usage{
qgraph.pca( cor, factors=1, rotation="promax", ...)
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

Revelle, W. (2010) psych: Procedures for Personality and Psychological Research Northwestern University, Evanston, http://personality-project.org/r/psych.manual.pdf, 1.0-93
}
\author{
Sacha Epskamp (s.epskamp@uva.nl)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.efa}}
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
qgraph.pca(cor(Y),5,vsize=c(1,10))

# Show crossloadings:
qgraph.pca(cor(Y),5,crossloadings=T,cut=0,vsize=c(1,10))

# Wider plot with larger nodes:
Q=qgraph.pca(cor(Y),5,vsize=c(2,10),width=17)

# With measurement model:
groups=list(1:20,21:40,41:60,61:80,81:100)
names(groups)=LETTERS[1:5]

qgraph.pca(cor(Y),5,,Q,groups=groups,legend=FALSE)

# Circulair layout:
qgraph.pca(cor(Y),5,vsize=c(2,10),layout="circle",groups=groups,
	legend=FALSE)
	
}}