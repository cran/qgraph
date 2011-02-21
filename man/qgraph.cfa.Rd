\name{qgraph.cfa}
\alias{qgraph.cfa}

\title{
CFA using Structural Equation Modelling
}
\description{
This function performs a simple confirmatory factor analysis using sem (Fox, 2010)
}
\usage{
qgraph.cfa(S, N, groups=NULL, labels=NULL, fun = "qgraph.loadings", ...)
}
\arguments{
  \item{S}{A covariance matrix}
  \item{N}{The number of observations}
  \item{groups}{The groups list, see \code{\link{qgraph}}. This must be a list in which each element is a factor indicating which variables load on the same factor}
  \item{labels}{A vector indicating the label of each variable}
  \item{fun}{A character indicating which function the results must be sent to. This can be "qgraph.loadings" to plot the standardized parameter estimates, "qgraph.semModel" to plot the path diagram, "qgraph.sem" to create full output report or any function that can handle a "sem" object (e.g. "summary")}
  \item{\dots}{Arguments passed to 'fun'}
  }
\details{
This function can be used to perform a simple confirmatory factor analasys using regular qgraph input. 
The function computes a model and then sends it to \code{\link{sem}} (Fox, 2010), and finally returns a "sem" 
object that can be used for manual inspection or to sent to \code{\link{qgraph.sem}}.
 
The model that is estimated is a first order factor model in which each variable loads on one factor and the factors 
are correlated. This model is specified with the `groups` argument. This must be a list in which each element 
represents a factor. Each element of the list must be a vector indicating which variables load on the same factor. 
The model is identified by fixing the first loading of each factor to 1, which should be an identifying restriction 
if there are at least 4 variables per factor.

The function returns a "sem" object, but also sends its results to another function for visualization. If this is 
\code{\link{qgraph.loadings}}, the default, then  a visualization of the standardized coeficients is plotted.
}
\value{
A "sem" object, see \code{\link{sem}}
}
\references{
John Fox with contributions from Adam Kramer <jfox@mcmaster.ca> and Michael Friendly (2010). sem: Structural Equation Models. R package version 0.9-21. http://CRAN.R-project.org/package=sem
}
\author{
Sacha Epskamp (s.epskamp@uva.nl)
}

\seealso{
\code{\link{qgraph}}
\code{\link{qgraph.sem}}
\code{\link{qgraph.loadings}}
\code{\link{qgraph.semModel}}
\code{\link{sem}}
}
\examples{
\dontrun{
set.seed(2)
eta<-matrix(rnorm(200*5),ncol=5)
lam<-matrix(rnorm(50*5,0,0.15),50,5)
lam[apply(diag(5)==1,1,rep,each=10)]<-rnorm(50,0.7,0.3)
th<-matrix(rnorm(200*50),ncol=50)
Y<-eta%*%t(lam)+th

gr<-list(1:10,11:20,21:30,31:40,41:50)

res <- qgraph.cfa(cov(Y),N=200,groups=gr,vsize=c(2,10))

qgraph.semModel(res,edge.label.cex=0.6)

qgraph(res,edge.label.cex=0.6)

qgraph.sem(res,filename="sem\%03d",onefile=F,panels=2,legend=FALSE,groups=gr,edge.label.cex=0.6)
}
}
\keyword{qgraph}
\keyword{sem}
\keyword{cfa}
