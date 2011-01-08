\name{qgraph.loadings}
\alias{qgraph.loadings}

\title{qgraph.loadings}
\description{
This function is a wrapper function for \code{\link{qgraph}} designed to visualize factor loadings.
}
\usage{
qgraph.loadings( fact, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{fact}{A matrix containing factor loadings (items per row, factors per column)}
	\item{...}{Additional optional arguments passed to \code{\link{qgraph}} and special arguments used in this function (described below).}
}
\section{Additional optional arguments}{	
\describe{
	\item{layout}{If "default" a standard layout for factor models will be made. If this is "circle" the default layout is circled (factors in the centre, items at the edge). No other layouts are currently supported.}
	\item{factors}{A vector where the first value indicates the size of manifest variables and the second value indicates the size of latent variables.}
	\item{model}{"reflective" to have arrows go to manifest variables, "formative" to have arrows go to latent variables or "none" (default) for no arrows}
	\item{crossloadings}{Logical, if TRUE then for each manifest variable the strongest loading is omitted (default to FALSE).}
	\item{groups}{An optional list containing the measurement model, see  \code{\link{qgraph}}}
	\item{Fname}{When there is only one factor, this is it's name. If there are more factors, the names in the groups list are used only if the factors can be identified.}
}}

\references{
https://sites.google.com/site/qgraphproject
}
\author{
Sacha Epskamp (s.epskamp@uva.nl)
}
\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.pca}}
\code{\link{qgraph.efa}}
}
\examples{
\dontrun{
#### VISUALIZE CORRELATION MATRIX ###
eta=matrix(rnorm(200*5),ncol=5)
lam=matrix(0,nrow=100,ncol=5)
for (i in 1:5) lam[(20*i-19):(20*i),i]=rnorm(20,0.7,0.3)
eps=matrix(rnorm(200*100),ncol=100)
Y=eta\%*\%t(lam)+eps

loadings=loadings(factanal(covmat=cor(Y),factors=5))[1:100,1:5]
qgraph.loadings(loadings)
}
}
