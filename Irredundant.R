# This function gets a CNF or DNF, and remove redundant monomials/clauses
Irredundant <- function(QQ)
{
  #   browser()
  QQ <- lapply(QQ, function(x) unique(unlist(x)))
  QQ <- QQ[order(sapply(QQ, length))]# Sort QQ in increasing length of terms
  
  check <- TRUE
  ind <- 1
  
  while(check==TRUE)
  {
    A <- length(QQ)
    rem.ind <- NULL
    #     check <- FALSE
    for (j in 1:A)
    {
      if (ind!=j) 
      {
        if(all(QQ[[ind]] %in% QQ[[j]])) 
          rem.ind <- c(rem.ind, j)
      }
    }
    if(length(rem.ind)>0) 
    {
      QQ[rem.ind] <- NULL
      check <- TRUE
    }
    ind <- ind+1
    if (ind > length(QQ)) 
      check <- FALSE
  }  
  
  
  return(QQ)
}
