Jaccard <- function(Net1, Net2)
{
  qgraph2adjacency <- function(graph, weighted = TRUE)
  {
    # Extract edgelist:
    E <- graph[['Edgelist']]
    
    # Number of nodes:
    n <- graph[['graphAttributes']][['Graph']][['nNodes']]
    
    ## Convert to adjacency:
    W <- matrix(0,n,n)
    for (i in 1:length(E$from))
    {
      if (E$weight[i]!=0)
      {
        W[E$from[i],E$to[i]] <- E$weight[i]
        if (!E$directed[i] | E$bidir[i]) W[E$to[i],E$from[i]] <- E$weight[i]
      }
    }
    
    if (!weighted)
    {
      W <- 1*(abs(W)>1e-10)
    }
    return(W)
  }
  
  if (is(Net1,"qgraph")) Net1 <- qgraph2adjacency(Net1, FALSE)
  if (is(Net2,"qgraph")) Net2 <- qgraph2adjacency(Net2, FALSE)
  
  stopifnot(isSymmetric(Net1) & isSymmetric(Net2))
  sum((Net1==1 & Net2==1)[upper.tri(Net1)]) / sum((Net1==1 | Net2==1)[upper.tri(Net1)]) 
}