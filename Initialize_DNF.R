Initialize_DNF <- function(method, cnf)
{
  if(method=='random')
  {
    set.seed(sample(1:1000, 1))
    dnf <- list()
    dnf <- unique(unlist(sapply(cnf, function(x) sample(x,1))))
  }
  
  if (method=='combination')
  {
    #   - for k from 1 to n (where n = number of variables)
    #   - for each possible combination c consisting of k out of n variables
    #   - check whether c is a valid DNF clause by checking that the intersection between every CNF clause and c is non-empty
    #   - if c is valid, add it to the DNF and break the loops.
    
    all.vars <- sort(unique(unlist(cnf)))
    all.vars <- all.vars[!is.na(all.vars)]
    var.num <- length(all.vars)
    
    k=1
    for (k in 1:var.num)
    {
      c <- combn(all.vars, k)
      for ( m in 1:ncol(c))
      {
        shared <- lapply(cnf, function(x) intersect(x, c[,m]))
        len <- sapply(shared, length)
        if (sum(len>0)==length(cnf)) 
        {
          dnf <- c[,m] 
          break
        }
        
      }
      
    }
  }
  return(dnf)
}