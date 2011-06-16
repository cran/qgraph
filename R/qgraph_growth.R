

qgraph.growth <- function(adj,ind=NULL,...,titles=NULL,sleep=0)
{


arguments <- list(...)

if (length(arguments)>0)
{
	for (i in 1:length(arguments))
	{
		if (class(arguments[[i]])=="qgraph") 
		{
			if (!is.null(names(arguments[[i]])))
			{
				for (j in 1:length(arguments[[i]]))
				{
					if (!(names(arguments[[i]])[j]%in%names(arguments)))
					{
						arguments[length(arguments)+1]=arguments[[i]][j]
						names(arguments)[length(arguments)]=names(arguments[[i]])[j]
					}
				}
			}
		}
	}
}

adjIsList <- is.list(adj)


if (!adjIsList && nrow(adj)!=ncol(adj)) stop("input must be an adjacency matrix or list of adjacency matrices")

n <- nrow(adj)
if (is.data.frame(ind)) ind <- as.matrix(ind)

if (is.null(ind) & !adjIsList)
{
	degs <- order((rowSums(adj)+colSums(adj))/2,decreasing=TRUE)
	ind <- matrix(FALSE,n,n)

	for (i in seq(n)) ind[i, degs[seq(i)]] <- TRUE
}

if (is.logical(ind) && length(ind)==n)
{
	sub <- ind
	meanDeg <- (rowSums(adj)+colSums(adj))/2
	meanDeg[sub] <- -Inf
	degs <- order(meanDeg,decreasing=TRUE)

	ind <- matrix(FALSE,n-sum(sub)+1,n)
	
	ind[,sub] <- TRUE

	for (i in seq(n-sum(sub))) ind[i+1, degs[seq(i)]] <- TRUE
}

if (is.numeric(ind)) ind <- as.list(ind)

if (is.list(ind))
{
	indList <- ind
	if (!all(sapply(indList,is.numeric))) stop("Indexes must be numeric in a list")
	ind <- matrix(FALSE,length(indList),n)
	for (i in seq(nrow(ind))) ind[seq(i,nrow(ind)),indList[[i]]] <- TRUE
}


if (!is.logical(ind)) stop("ind must be logical")
if (ncol(ind) != n) stop("Number of columns in ind must correspond to total number of nodes")

# Start the loop:
sub <- NULL

for (i in seq(nrow(ind)))
{

	subOld <- sub
	sub <- ind[i,]
	
	adjSub <- adj[sub,sub]
	
	initNew <- matrix(rnorm(2*sum(sub)),sum(sub),2)
	if (!is.null(subOld)) initNew[subOld[sub],] <- init
	
	layout.par <- list(max.delta = rep(n,sum(sub)), area = n^2.3, repulse.rad = n^2.8,
		init = initNew)
	
	if (!is.null(subOld)) layout.par$max.delta[subOld[sub]] <- n/10
	
	arg2 <- lapply(arguments,function(x)if(length(x)==n)x[sub] else if (is.matrix(x) && ncol(x)==n&&nrow(x)==n)x[sub,sub] else x)
	class(arg2) <- "qgraph"

	# Run qgraph:
	init <- qgraph(adjSub,layout="spring",layout.par=layout.par,arg2)$layout.orig
	
	if (!is.null(titles)) title(titles[i],line=-1)
	Sys.sleep(sleep)
}
}
