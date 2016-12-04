DNFGenerating_FK <- function(cnf, split.method, infinite.thresh=10, dnf.init)
{
#      browser()
  #cnf=list(c(1,2), c(3,4,5), c(1,6,7), c(2,5), c(4,6))
  
  source("FK_B.R")
  source("Initialize_DNF.R")
  
  check <- 1
  
  dnf <- list()
  # start point of DNF
  dnf[[1]] <- Initialize_DNF(method=dnf.init, cnf)
  dnf.len <- 1
  Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE)
  #   while (Assign==F)
  #   {
  #     dnf[[1]] <- RestartDNF(cnf)
  #     Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE)
  #   }
  
  if (is.element(0, Assign)) Assign <- setdiff(Assign, 0)
  iter <- 0
  infinite.counter <- 0
  
  while(length(Assign)>0 & check==1)
  {
    iter <- iter+1
    n <- length(dnf)
    dnf[[n+1]] <- Assign
    dnf <- Irredundant(dnf)
    dnf.len[iter+1] <- length(dnf)
    Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE, split.method= split.method)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++
    #     When output of FK_B is FALSE, we restart
    #++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (length(Assign)==1)
      if (Assign==F) 
      {
        infinite.counter <- infinite.counter+1
        
        if(infinite.counter > infinite.thresh)
        {
          check <- 0
          break
        }
        
        #   Restart everything
        dnf <- list()
        dnf[[1]] <- Initialize_DNF(method=dnf.init, cnf)
        dnf.len <- 1
#         browser()
        Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE, split.method= split.method)
        if (is.element(0, Assign)) Assign <- setdiff(Assign, 0)
        iter <- 0
        
      }
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #     When dnf.len has not changed in more than 10 iterations we restart
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (iter>10)
    {
      n <- length(dnf.len)
      if(length(unique(dnf.len[(n-9):n]))==1)
      {
        infinite.counter <- infinite.counter+1
        if(infinite.counter > infinite.thresh)
        {
          check <- 0
          break
        }
        
        #   Restart everything
        dnf <- list()
        dnf[[1]] <- Initialize_DNF(method=dnf.init, cnf)
        dnf.len <- 1
        Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE, split.method= split.method)
        if (is.element(0, Assign)) Assign <- setdiff(Assign, 0)
        iter <- 0
      }
    }
    
    if (is.element(0, Assign)) Assign <- setdiff(Assign, 0)
    
    ind <- which(sapply(dnf, length)==length(Assign))
    for (i in ind)
    {
      if (all(dnf[[i]]==Assign)) #Restart 
      {
        infinite.counter <- infinite.counter+1
        
        if(infinite.counter > infinite.thresh)
        {
          check <- 0
          break
        }
        
        #   Restart again
        dnf <- list()
        dnf[[1]] <- Initialize_DNF(method=dnf.init, cnf)
        dnf.len <- 1
        Assign <- FK_B(cnf=cnf, dnf=dnf, verbose=FALSE, split.method= split.method)
        if (is.element(0, Assign)) Assign <- setdiff(Assign, 0)
        iter <- 0
#         browser()
        break
      }
    }   
  }
  
#   if (check==0)
#     stop("INFINITE loop!!!")
  return(list(dnf=dnf, iteration=iter, infinite.counter=infinite.counter, dnf.len=dnf.len, Solved=check))
}
