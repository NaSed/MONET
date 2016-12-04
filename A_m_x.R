# This function generates D^{m,x}_1 or C^{m,x}_0 by setting the variables in m to TRUE and calculate the remaining formula.
# Please see "how to apply sat-solving for the equivalence test of mootone normal forms", page 4
# This function inputs are:
# 1) A: D_x_1 or C_x_0
# 2) c: the set of variables which must be set to TRUE
# 3) type: It indicates to the form of A; It can be CNF or DNF
# Returns D^{m,x}_1 or C^{m,x}_0
A_m_x <- function(A, m, type)
{
  # If A is in the form of CNF, we should remove whole terms that include the variables in m
  # If A is in the form of DNF we should remove the variables in m from all terms
  
  if (type=='DNF')
  {
    for (i in 1:length(m))
    {
      z <- m[i]
      t <- which(sapply(A, function(x) is.element(z, x)))
      
      if(length(t)>0) # z is available in at least one term in A (DNF type)
      {
        for(j in t) # Removing z from each term
          A[[j]] <- setdiff(A[[j]],z)
      }
    }# for each variable in c we set it false in A
  }else#(type=='CNF')
  {    
    for (i in 1:length(m))
    {
      if(length(A)>0)
      {
        z <- m[i]
        t <- which(sapply(A, function(x) is.element(z, x)))
        # We remove the wole terms that include z
        A[t] <- NULL
      }
    }# for each variable in c we set it false in A
  }# end else
  
#   if (length(A)==0)
#     cat("There is no more terms in A after removing variables included in m. Here is A_m_x() function.")
  len <- sapply(A, length)
  
  if (is.element(0, len))
    A[which(len==0)] <- NULL
#   
#   if (length(A)==0)
#     cat("There is no more terms in A after removing variables included in m. Here is A_m_x() function.")
  
  return(A)
}