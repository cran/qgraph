## qgraph.efa

# EXPLORATORY FACTOR ANALYSIS
# Using factanal(..)

# all arguments of qgraph.loadings are included. plus:

# cor: correlation matrix on which to perform the EFA.
# factors: vector with how much factors are to be used
# rotation: rotations to be used.

qgraph.efa=function(cor,factors=1,rotation="promax",...) {

fact=factanal(covmat=cor,factors=factors,rotation=rotation)
loadings=as.matrix(loadings(fact)[1:nrow(cor),1:factors])

qgraph.loadings(loadings,model="reflective",...) 
	

}