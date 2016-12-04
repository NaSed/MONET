# This function calculates the value of DNF by assigning vals to the variables
# vals is like the below:
# V1 V2 V3 V4
# 0  0   1  1

DNF_value <- function(dnf, vals)
{
  new.dnf <- list()
#   browser()
  for(i in 1:length(dnf))
  {
    monomial <- dnf[[i]]
    zero.vars <- as.numeric(names(which(vals==0)))
    one.vars <- as.numeric(names(which(vals==1)))
    
    one.monomial <- intersect(monomial, one.vars)
    zero.monomial <- intersect(monomial, zero.vars)
    
    if(length(zero.monomial)>0)
    {
      new.dnf[[i]] <- NA # it means that the monomial is FALSE
    }else
    {
      if(length(one.monomial)>0)
        temp <- setdiff(monomial, one.vars)
      if(length(temp)==0) # all of vars in a monomial are true
        return(TRUE)
      else new.dnf[[i]] <- temp
    }
  }
  na.ind <- sapply(new.dnf, function(x) if(length(x)==1) is.na(x) else return(FALSE)) # these are monomials that are FALSE, then we can surely remove them from DNF
  new.dnf[which(na.ind==TRUE)] <- NULL
  if(length(new.dnf)==0) 
    return(FALSE)
  else return(TRUE)
}