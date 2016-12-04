# This function controls equivalence of CNF and DNF by enumeration and making truth table
Easy_case <- function(dnf, cnf)
{
  vars <- sort(unique(c(unlist(dnf),unlist(cnf))))
  
  n <- length(vars)
  
  # Making Binary table for n variables
  table <- matrix(0, nrow=2^n, ncol=n)
  
  colnames(table) <- vars
  
  for( i in 0:(ncol(table)-1))
    table[,(ncol(table)-i)] <- rep(c(0,1), times= nrow(table)/(2^(i+1)), each=(2^i))
  
  
  # Now, we put each row in table in CNF and DNF, then calculate their output value
  
  CNF.val <- rep(NA, nrow(table))
  DNF.val <- rep(NA, nrow(table))
  
  for(i in 1:nrow(table))
  {
    if(length(cnf)>0) CNF.val[i] <- 1 * CNF_value(cnf=cnf, vals=table[i, 1:n]) else CNF.val[i] <- 1
    if(length(dnf)>0) DNF.val[i] <- 1 * DNF_value(dnf=dnf, vals=table[i, 1:n]) else DNF.val[i] <- 1
  }
  if (all(CNF.val==DNF.val)) 
    return('TRUE') 
  else{
    #keep those rwos that CNF and DNF are different
    table <- table[which(CNF.val+DNF.val==1),]
    if(is.matrix(table))
    {
      l <- apply(table, 1, sum)
#       l <- table[which.max(l),]
      l <- table[sample(1:length(l), 1),]
      var <- as.numeric(colnames(table)[which(l==1)])
#       browser()
#       var <- as.numeric(colnames(table)[which.max(l)[1]]) #Find conflict variables which is one in all conflicts
    }else var <- vars[which(table==1)]
    return(var)
  } 
}