\name{qgraph.layout}
\alias{qgraph.layout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{qgraph.layout}
\description{
This function runs \code{\link{qgraph}} but does not plot the graph and returns the layout. It is used in \code{\link{qgraph.sem}}.
It is recommended to not use this function but use \code{\link{qgraph}} instead (which also returns the layout as part of the output).
}
\usage{
qgraph.layout( adj, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{adj}{The input, see \code{\link{qgraph}}}
	\item{...}{Other arguments used in \code{\link{qgraph}}}
}

\references{
https://sites.google.com/site/qgraphproject
}
\author{
Sacha Epskamp (s.epskamp@uva.nl)
}
\seealso{
\code{\link{qgraph}}
}
