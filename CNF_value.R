# This function calculates the value of CNF by assigning vals to the variables
# vals is like the below:
# V1 V2 V3 V4
# 0  0   1  1

CNF_value <- function(cnf, vals)
{
  new.cnf <- list()
#   browser()
  for(i in 1:length(cnf))
  {
    clause <- cnf[[i]]
    zero.vars <- as.numeric(names(which(vals==0)))
    one.vars <- as.numeric(names(which(vals==1)))
    
    one.cluase <- intersect(clause, one.vars)
    zero.clause <- intersect(clause, zero.vars)
    
    if(length(one.cluase)>0)
    {
      new.cnf[[i]] <- NA # it means that the cluase is TRUE
    }else
    {
      if(length(zero.clause)>0)
        temp <- setdiff(clause, zero.vars)
      if(length(temp)==0) # all of vars in a clause are false
        return(FALSE)
      else new.cnf[[i]] <- temp
    }
  }
  na.ind <- sapply(new.cnf, function(x) if(length(x)==1) is.na(x) else return(FALSE)) # these are clauses that are TRUE, then we can surely remove them from CNF
  new.cnf[which(na.ind==TRUE)] <- NULL
  if(length(new.cnf)==0) 
    return(TRUE)
  else return(FALSE)
}