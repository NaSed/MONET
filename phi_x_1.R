# phi_x_1 denotes the formula that consists of all terms of phi that do not contain x.
phi_x_1 <- function(phi, split.var)
{
#   browser()
  t <- which(sapply(phi, function(x) !is.element(split.var, x)))
  
  return(phi[t])
    
}