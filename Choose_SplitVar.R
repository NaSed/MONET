Choose_SplitVar <- function(cnf, dnf, method)
{
  # This function selects variable x which is most frequent in both CNF and DNF
  #   browser()
  if(method=='mostFreq')
  {
    cnf.var <- unlist(cnf)
    dnf.var <- unlist(dnf)
    cnf.d <- data.frame(table(cnf.var))
    cnf.d <- cnf.d[order(-cnf.d$Freq),]
    
    dnf.d <- data.frame(table(dnf.var))
    dnf.d <- dnf.d[order(-dnf.d$Freq),]
    
    for ( i in 1:nrow(cnf.d))
    {
      var <- intersect(cnf.d[1:i,1], dnf.d[1:i,1])
      if (length(var)>0)
        return(as.numeric(var[1]))
    }
  }
  if(method=='lessFreq')
  {
    cnf.var <- unlist(cnf)
    dnf.var <- unlist(dnf)
    cnf.d <- data.frame(table(cnf.var))
    cnf.d <- cnf.d[order(cnf.d$Freq),]
#     cnf.d <- cnf.d[-which(cnf.d$Freq==0),]
    
    dnf.d <- data.frame(table(dnf.var))
    dnf.d <- dnf.d[order(dnf.d$Freq),]
#     dnf.d <- dnf.d[-which(dnf.d$Freq==0),]
    
    for ( i in 1:nrow(cnf.d))
    {
      var <- intersect(cnf.d[1:i,1], dnf.d[1:i,1])
      if (length(var)>0)
        return(as.numeric(var[1]))
    }
  }  
  if(method=='mostFreqSum')
  {
    cnf.var <- unlist(cnf)
    dnf.var <- unlist(dnf)
    cnf.d <- data.frame(table(cnf.var))
    dnf.d <- data.frame(table(dnf.var))
    
    all.d <- cbind(cnf.d[,1], (cnf.d$Freq+dnf.d$Freq))
    all.d <- as.matrix(all.d)
    all.d <- all.d[order(-all.d[,2]),]
    var <- all.d[1,1]
    if (length(var)>0)
      return(as.numeric(var))
  }
  
  if(method=='adaptive')
  {
    cnf.var <- unlist(cnf)
    dnf.var <- unlist(dnf)
    cnf.d <- data.frame(table(cnf.var))
    cnf.d <- cnf.d[order(-cnf.d$Freq),]
    
    dnf.d <- data.frame(table(dnf.var))
    dnf.d <- dnf.d[order(-dnf.d$Freq),]
    
    for ( i in 1:min(2,nrow(cnf.d)))# we just check the first 3 most frequent variables.
    {
      var <- intersect(cnf.d[1:i,1], dnf.d[1:i,1])
      if (length(var)>0)
        return(as.numeric(var[1]))
    }
    all.var <- unique(c(cnf.var, dnf.var))
    var <- sample(all.var, 1)
    
  }# end if(method=='adaptive)
  if (length(var)==0)  stop("check Choose_SplitVar function!!!")
  else res <- var
  
  return(res)
}