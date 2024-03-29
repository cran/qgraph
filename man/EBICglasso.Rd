\name{EBICglasso}
\alias{EBICglasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Gaussian graphical model using graphical lasso based on extended BIC criterium.
}
\description{
This function uses the \code{\link[glasso]{glasso}} package (Friedman, Hastie and Tibshirani, 2011) to compute a sparse gaussian graphical model with the graphical lasso (Friedman, Hastie and Tibshirani, 2008). The tuning parameter is chosen using the Extended Bayesian Information criterium  (EBIC). 
}


\usage{
EBICglasso(S, n, gamma = 0.5, penalize.diagonal = FALSE, nlambda = 100, 
            lambda.min.ratio = 0.01, returnAllResults = FALSE, checkPD = TRUE, 
            penalizeMatrix, countDiagonal = FALSE, refit = FALSE, threshold = FALSE,
            verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
A covariance or correlation matrix
}
  \item{n}{
Sample size used in computing \code{S}
}
  \item{gamma}{
EBIC tuning parameter. 0.5 is generally a good choice. Setting to zero will cause regular BIC to be used.
}
  \item{penalize.diagonal}{
Should the diagonal be penalized? 
}
  \item{nlambda}{
Number of lambda values to test.
}
  \item{lambda.min.ratio}{
Ratio of lowest lambda value compared to maximal lambda
}
  \item{returnAllResults}{
  If \code{TRUE} this function does not return a network but the results of the entire glasso path.
  }
  \item{checkPD}{
  If \code{TRUE}, the function will check if \code{S} is positive definite and return an error if not. It is not advised to use a non-positive definite matrix as input as (a) that can not be a covariance matrix and (b) glasso can hang if the input is not positive definite.
  }
  \item{penalizeMatrix}{
  Optional logical matrix to indicate which elements are penalized
  }
  \item{countDiagonal}{
    Should diagonal be counted in EBIC computation? Defaults to \code{FALSE}. Set to \code{TRUE} to mimic qgraph < 1.3 behavior (not recommended!).
  }
  \item{refit}{
    Logical, should the optimal graph be refitted without LASSO regularization? Defaults to \code{FALSE}.
  }
 \item{threshold}{
    Logical, should elements of the precision matrix that are below (log(p*(p-1)/2)) / sqrt(n) be removed (both before EBIC computation and in final model)? Set to \code{TRUE} to ensure high specificity.
  }
  \item{verbose}{
    Logical, should progress output be printed to the console?
  }
  \item{\dots}{
Arguments sent to \code{\link[glasso]{glasso}}
}
}
\details{
The glasso is run for 100 values of the tuning parameter logarithmically spaced between the maximal value of the tuning parameter at which all edges are zero, lamba_max, and lambda_max/100. For each of these graphs the EBIC is computed and the graph with the best EBIC is selected. The partial correlation matrix is computed using \code{\link{wi2net}} and returned. When \code{threshold = TRUE}, elements of the inverse variance-covariance matrix are first thresholded using the theoretical bound (Jankova and van de Geer, 2018).
}
\value{
A partial correlation matrix
}
\references{
Friedman, J., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9(3), 432-441.
Chicago  

Jerome Friedman, Trevor Hastie and Rob Tibshirani (2011). glasso: Graphical lasso-estimation of Gaussian graphical models. R package version 1.7.
http://CRAN.R-project.org/package=glasso

Foygel, R., & Drton, M. (2010, November). Extended Bayesian Information Criteria for Gaussian Graphical Models. In NIPS (pp. 604-612).
Chicago  

Revelle, W. (2014) psych: Procedures for Personality and Psychological Research,
Northwestern University, Evanston, Illinois, USA,
http://CRAN.R-project.org/package=psych Version = 1.4.4.

 Bates, D., and Maechler, M. (2014). Matrix: Sparse and Dense Matrix Classes and
Methods. R package version 1.1-3. http://CRAN.R-project.org/package=Matrix

Jankova, J., and van de Geer, S. (2018) Inference for high-dimensional graphical models. In: Handbook of graphical models (editors: Drton, M., Maathuis, M., Lauritzen, S., and Wainwright, M.). CRC Press: Boca Raton, Florida, USA.
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}
\examples{
\dontrun{
### Using bfi dataset from psych ###
library("psych")
data(bfi)

# Compute correlations:
CorMat <- cor_auto(bfi[,1:25])

# Compute graph with tuning = 0 (BIC):
BICgraph <- EBICglasso(CorMat, nrow(bfi), 0, threshold = TRUE)

# Compute graph with tuning = 0.5 (EBIC)
EBICgraph <- EBICglasso(CorMat, nrow(bfi), 0.5, threshold = TRUE)

# Plot both:
layout(t(1:2))
BICgraph <- qgraph(BICgraph, layout = "spring", title = "BIC", details = TRUE)
EBICgraph <- qgraph(EBICgraph, layout = "spring", title = "EBIC")

# Compare centrality and clustering:
layout(1)
centralityPlot(list(BIC = BICgraph, EBIC = EBICgraph))
clusteringPlot(list(BIC = BICgraph, EBIC = EBICgraph))
}
}

