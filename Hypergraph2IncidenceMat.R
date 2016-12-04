# This script converts a hypergraph showed as
# 2, 3, 4
# 1, 2, 5
# 1, 3
# to an incidence matrix as
# 0 1 1 1 0
# 1 1 0 0 1
# 1 0 1 0 0
# Input, h, is a list
Hypergraph2IncidenceMat <- function(h)
{
  # Number of variables
  nvar <- length(unique(unlist(h)))
  
  # Number of hyperedges
  nedge <- length(h)
  
  vars <- sort(unique(unlist(h)))
  
  mat <- matrix(0, nrow=nedge, ncol=nvar)
  colnames(mat) <- paste("V", vars, sep='')
  
  for (i in 1:nedge)
    mat[i, paste("V", unlist(h[[i]]), sep='')] <- 1
  
  return(as.matrix(mat))
  
}