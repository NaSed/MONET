# phi_x_0 denotes the formula that consists of terms of phi from which x is removed
phi_x_0 <- function(phi, split.var)
{
  
  #   browser()
  t <- which(sapply(phi, function(x) is.element(split.var, x)))
  phi <- phi[t]
  if(length(phi)==0) return(NULL)
  for (i in 1:length(phi))
    phi[[i]] <- setdiff(phi[[i]],split.var)
  
  len <- sapply(phi, length)
  ind <- which(len==0)
  
  # remove those that have no terms anymore
  if (length(ind)>0)
    phi[[ind]] <- NULL
  
  return(phi)
  
}