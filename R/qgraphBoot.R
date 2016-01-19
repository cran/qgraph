estimateNetwork <- function(
  data,
  corFunction = cor_auto, # Function to produce the correlation or covariance matrix
  corArgs = list(), # list with arguments for the correlation function
  graphFunction = EBICglasso, # function that results in a network
  graphArgs
){
  
  if (missing(graphArgs)){
    if ((is.character(graphFunction) && graphFunction == "EBICglassp") || deparse(substitute(graphFunction)) == "EBICglasso"){
      graphArgs <- list(n = nrow(data))
    }
  }
  
  # Compute cov matrix:
  covMat <- do.call(corFunction, c(list(data), corArgs))
  
  # Compute network:
  net <- do.call(graphFunction, c(list(covMat),graph))
  
  # Return network:
  return(net)
}

qgraphBoot <- function(
  data, # Dataset
  nBoots = 1000, # Number of bootstrap samples.
  corFunction = cor_auto, # Function to produce the correlation or covariance matrix
  corArgs = list(), # list with arguments for the correlation function
  graphFunction = EBICglasso, # function that results in a network
  graphArgs # arguments sent to the graph estimation function (if missing automatically sample size is included)
  ){

  
  # Estimate sample graph:
  sampleNet <- estimateNetwork(data, corFunction, corArgs, graphFunction, graphArgs)
  
  browser()
  
}