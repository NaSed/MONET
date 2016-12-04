# In this function one of CNF (DNF) is NULL (or logically TRUE) and we aim to find the smallest conflict assignment, i.e an
# assignment that makes DNF (CNF) false, then makes CNF and DNF unequivalent.
MinimumConflictAssignment <- function(CNF, DNF)
{
#   browser()
  if (length(CNF)>0 & length(DNF)>0)
    stop("Both of CNF and DNF are not NULL! The MinimumConflictAssignment function is applicable when one of CNF or DNF is NULL.")
  
#   if(length(CNF)>0) # DNF is NULL
#   {
#     # sometimes CNF only includes one clause, in this case all of variables must be zero to make it FALSE
#     if(!is.null(dim(CNF)))
#     {
#       vars.1 <- 100* CNF
#       browser()
#     }else{    
#       # find the biggest clause
#       ind <- which.min(sapply(CNF, length))
#       vars.0 <- CNF[[ind]] # variables that must be zero in conflict assignment
#       all.vars <- unlist(CNF)
#       rem.vars <- setdiff(all.vars, vars.0)
#       if(length(rem.vars)>0)
#         #         vars.1 <- sample(rem.vars, 1) # this var is TURE in conflict assignmet, the remainings are FALSE
#         vars.1 <- rem.vars # this var is TURE in conflict assignmet, the remainings are FALSE        
#       else vars.1 <- FALSE
#     }
#   }else{  #CNF is NULL
#     len <- sapply(DNF, length)
#     ind <- which(len==1)
#     if (length(ind)>0)  vars.0 <- unique(unlist(DNF[ind])) else vars.0 <- NULL
#     
#     ind <- which(len>=2) # find monomials with more than one variable; monomials with only one variables must be zero
#     DNF <- DNF[ind]
#     all.vars <- unlist(DNF)
#     rem.vars <- setdiff(all.vars, vars.0)    
#     if(length(rem.vars)>0)
#       vars.1 <- sample(rem.vars, 1) # this var is TURE in conflict assignmet, the remainings are FALSE
#     else vars.1 <- FALSE
#   }
  
  return(vars.1=FALSE)
}