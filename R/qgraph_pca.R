## qgraph.pca

# EXPLORATORY FACTOR ANALYSIS
# Using factanal(..)

# all arguments of qgraph.loadings are included. plus:

# cor: correlation matrix on which to perform the EFA.
# factors: vector with how much factors are to be used
# rotation: rotations to be used.

qgraph.pca=function(cor,factors=1,rotation="promax",...) {

require(psych)

fact=principal(cor,factors,rotate=rotation)
loadings=as.matrix(loadings(fact)[1:nrow(cor),1:factors])

Q=qgraph.loadings(loadings,model="formative",...) 
	
invisible(Q)
}