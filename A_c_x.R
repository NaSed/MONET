# This function generates C^{c,x}_1 or D^{c,x}_0 by setting the variables in c to FALSE and calculate the remaining formula.
# Please see "how to apply sat-solving for the equivalence test of mootone normal forms", page 4
# This function inputs are:
# 1) A: C_x_1 or D_x_0
# 2) cc: the set of variables which must be set to FALSE
# 3) type: It indicates to the form of A; It can be CNF or DNF
# Returns C^{c,x}_1 or D^{c,x}_0
A_c_x <- function(A, cc, type)
{
  # If A is in the form of CNF, we should remove the variables in c from all terms
  # If A is in the form of DNF we should remove whole terms that include the variables in c
  if (type=='CNF')
  {
    for (i in 1:length(cc))
    {
      z <- cc[i]
      t <- which(sapply(A, function(x) is.element(z, x)))
      if(length(t)>0) # z is available in at least one term in A (CNF type)
      {
        for(j in t) # Removing z from each term
          A[[j]] <- setdiff(A[[j]],z)
      }
    }# for each variable in c we set it false in A
  }else{ #type=='DNF'
    for (i in 1:length(cc))
    {
      if(length(A)>0)
      {
        z <- cc[i]
        t <- which(sapply(A, function(x) is.element(z, x)))
        # We remove the wole terms that include z
        A[t] <- NULL
      }
    }# for each variable in c we set it false in A
  }# end else
  
#   if (length(A)==0)
#     cat("There is no more terms in A after removing variables included in c. Here is A_c_x() function.")
  
  len <- sapply(A, length)
  
  if (is.element(0, len))
    A[which(len==0)] <- NULL
  
#   if (length(A)==0)
#     cat("There is no more terms in A after removing variables included in c. Here is A_c_x() function.")
#   
  return(A)
}