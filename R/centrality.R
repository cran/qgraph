
centrality <- function(graph,alpha=1,posfun=abs,pkg = c("igraph","qgraph"),all.shortest.paths=FALSE,
                       weighted = TRUE, signed = TRUE, R2 = FALSE)
{

  # Check for correct class:
  #   if (class(graph) != "qgraph") stop("Must be a 'qgraph' object")
  
  #   if (!is.null(graph[['graphAttributes']][['Graph']][['weighted']])) if (!graph[['graphAttributes']][['Graph']][['weighted']]) graph[['Edgelist']][['weight']] <- ifelse(graph[['Edgelist']][['weight']]==0,0,1)
  
  #   if (!isTRUE(graph[['graphAttributes']][['Graph']][['minimum']] == 0))
  #   {
  #     warning("Minimum in graph is not set to zero. Omitted edges will not be included in computation of centrality measures.")
  #   }
  #   
  #   # Extract edgelist:
  #   E <- graph[['Edgelist']]
  #   
  #   # Number of nodes:
  #   n <- graph[['graphAttributes']][['Graph']][['nNodes']]
  #   
  #   ## Convert to adjacency:
  #   W <- matrix(0,n,n)
  #   for (i in 1:length(E$from))
  #   {
  #     if (E$weight[i]!=0)
  #     {
  #       W[E$from[i],E$to[i]] <- E$weight[i]
  #       if (!E$directed[i] | E$bidir[i]) W[E$to[i],E$from[i]] <- E$weight[i]
  #     }
  #   }
  
  W <- getWmat(graph)
  if (!isTRUE(weighted)){
    W <- sign(W)
  }
  
  if (!isTRUE(signed)){
    W <- abs(W)
  }
  
  pkg <- match.arg(pkg)
#   if (missing(pkg)){
#     pkg <- ifelse(all(W==t(W)),"igraph","qgraph")
#     
#   }
  # If is list, compute for all:
  if (is.list(W))
  {
    return(lapply(W,centrality, alpha=alpha,posfun=posfun))
  }
  
  n <- nrow(W)
  
  # Remove diagonal:
  if (any(diag(W)!=0))
  {
    #     message("Self-loops are not included in centrality analysis.")
    diag(W) <- 0 
  }
  
  ## Compute adjacency:
  X <- 1L * (W!=0)
  
  ## Compute default measures:
  UnweightedDegreesOut <- rowSums(X)
  WeightedDegreesOut <- rowSums(posfun(W))
  CombinedDegreesOut <- UnweightedDegreesOut^(1-alpha) * WeightedDegreesOut^alpha
  
  UnweightedDegreesIn <- colSums(X)
  WeightedDegreesIn <- colSums(posfun(W))
  CombinedDegreesIn <- UnweightedDegreesIn^(1-alpha) * WeightedDegreesIn^alpha
  
  # Expected Influence
  InExpectedInfluence <- colSums(W)
  OutExpectedInfluence <- rowSums(W)
  
  # # Randomized Shortest Paths Betweenness Centrality
  # rspbc <- NetworkToolbox::rspbc(abs(W))
  # 
  # # Hybrid Centrality
  # hybrid <- NetworkToolbox::hybrid(abs(W), BC = "random")
  
  DistMat <- 1/(ifelse(posfun(W)==0,0,posfun(W)^alpha))
  if (pkg=="igraph"){
    igraphObject <- igraph::graph.adjacency(DistMat, weighted = TRUE, mode = "directed")
    
#     E <- cbind(c(row(W)),c(col(W)),c(W))
#     E <- E[E[,3] != 0]
#     E[,3] <- 1/E[,3]
#     igraphObject <- igraph::graph_from_edgelist(E[,1:2],directed=TRUE)
#     E(igraphObject)$weight <- E[,3]
    
    Closeness <- igraph::closeness(igraphObject)

    E <- cbind(c(row(W)),c(col(W)),c(posfun(W)))
    # E <- E[E[,3] != 0, ]
    # E[,3] <- 1/E[,3]
    igraphObject <- igraph::graph_from_edgelist(E[,1:2, drop=FALSE],directed=TRUE)
    
    
    
    E(igraphObject)$weight <- 1/E[,3]
    igraphObject <- igraph::delete_edges(igraphObject, which(E(igraphObject)$weight == Inf))
    
    Betweenness <-  igraph::betweenness(igraphObject,cutoff = 1/1e-10)
    
    ShortestPaths <- igraph::shortest.paths(igraphObject, mode = "out")
    
    
    ls <- vector("list",n^2)
    Paths <- structure( ls, .Dim = c(n, n))
    
    if (all.shortest.paths){
      for (i in 1:n)
      {
        allPaths <- lapply(igraph::all_shortest_paths(igraphObject,i,V(igraphObject))$res,as.numeric)
        last <- sapply(allPaths,function(x)x[length(x)])
        
        for (j in 1:n)
        {
          if (i==j){
            Paths[[i,j]] <- list()
          } else {
            Paths[[i,j]] <-  allPaths[last==j]
          }
        }
      }
    }
    
  } else {
    # Compute shortest distance using Dijkstra (code based on pseudo code on Wikipedia)
    # Setup:
    
    ShortestPaths <- matrix(Inf,n,n)
    ls <- list()
    for (i in 1:n^2) ls[[i]] <- numeric(0)
    Previous <- structure(ls, .Dim = c(n, n))
    
    # Main loop:
    for (source in 1:n)
    {
      dist <- rep(Inf,n) 
      #previous <- integer(n)                # Previous node in optimal path from source
      dist[source] <- 0                     # Distance from source to source
      Q <- 1:n                              # All nodes in the graph are unoptimized - thus are in Q
      while (length(Q) > 0)                 # The main loop
      {
        u <- Q[which.min(dist[Q])]
        if (dist[u] == Inf)  break          # all remaining vertices are inaccessible from source
        Q <- Q[- which(Q==u)]
        for (v in Q)                        # where v has not yet been removed from Q.
        {
          alt <- dist[u] + DistMat[u,v] 
          if (alt < dist[v])                # Relax (u,v,a)
          {
            dist[v] <- alt 
            Previous[[source,v]] <- which(dist + DistMat[,v] == alt)
            #previous[v] <- u 
            #   decrease-key v in Q          # Reorder v in the Queue
          }
        }
      }
      ShortestPaths[source,] <- dist
    }
    
    # Compute Closeness:
    Closeness <- 1/rowSums(ShortestPaths)
    
    
    
    # Shortest paths function:
    sp <- function(i,j)
    {
      if (length(Previous[[i,j]])==0) return(list())
      if (all(Previous[[i,j]] == i)) return(list(c(i,j)))
      paths <- do.call(c,lapply(Previous[[i,j]],sp,i=i))
      paths <- lapply(paths,function(x)c(x,j))
      return(paths)
    }
    
    # Compute shortest paths:
    Paths <- structure(ls, .Dim = c(n, n))
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        Paths[[i,j]] <- sp(i,j)
      }
    }
    
    # Number of shortest paths:
    NumPaths <- apply(Paths,1:2,sapply,length)
    
    # Betweenness dummy:
    Betweenness <- numeric(n)
    
    Gtot <- apply(Paths,1:2,sapply,length)
    
    # Compute betweenness:
    for (i in 1:n)
    {
      G <- apply(Paths,1:2,sapply,function(x)sum(i==unlist(x)))
      Grat <- G[-i,-i]/Gtot[-i,-i]
      Betweenness[i] <- sum(Grat[!is.nan(Grat)])
    }
  }
  
  lab <- function(x,labs){
    if (is.vector(x)){
      names(x) <- labs
    } else {
      rownames(x) <- colnames(x) <- labs
    }
    return(x)
  }
  Labels <- colnames(W)
  
  # R2:
  if (R2){
    # check if the matrix could be a GGM:
    W <- as.matrix(W)
    diag(W) <- 0
    K <- diag(n) - W
    rownames(K) <- colnames(K) <- NULL
    if (any(K < -1) || any(K > 1) || !all(K == t(K))  || any(eigen(K)$values < 0)){
      stop("Graph does not look like a Gaussian graphical model. R2 is only supported for a Gaussian graphical model.")
    }
    
    # translate to precision matrix of standardized data:
    K <- solve(cov2cor(solve(K)))
    
    # R^2 is simply...
    R2_res <- 1 - 1 / diag(K)
    names(R2_res) <- Labels
  }
  
  ### RETURN VALUES:
  retval <- list(
    OutDegree = lab(CombinedDegreesOut,Labels),
    InDegree = lab(CombinedDegreesIn,Labels),
    Closeness = lab(Closeness,Labels),
    Betweenness = lab(Betweenness,Labels),
    # rspbc = lab(as.vector(rspbc),Labels),
    # hybrid = lab(as.vector(hybrid),Labels),
    InExpectedInfluence = InExpectedInfluence,
    OutExpectedInfluence = OutExpectedInfluence,
    ShortestPathLengths = lab(ShortestPaths,Labels),
    ShortestPaths = lab(Paths,Labels))
  
  if (R2){
    retval$R2 <- R2_res
  }
  
  return(retval)  
}
