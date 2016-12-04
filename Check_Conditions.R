# Checking conditions:
# 1: for each c in CNF and m in DNF => intersect(m, c) != null
# 2: DNF and CNF must have exactly the same variables
# 3: max{|m|: m \in DNF} <= |C|   &   max{|c|: c \in CNF} <= |D|

# Inputs: CNF and DNF as lists
Check_Conditions <- function(cnf, dnf, verbose)
{
#   browser()
  if(verbose) cat('\n', 'Checking conditions:', '\n', file=file, append=TRUE)
  
  Assign <- NULL
  # number of monomials in DNF
  n.d <- length(dnf)
  
  # number of clauses in CNF
  n.c <- length(cnf)
  
  #++++++++++++++++++++++++++++
  #
  #++++++++++++++++++++++++++++
  if (n.c == 0 | n.d == 0)
  {
    if(verbose) cat('CNF or DNF is NULL, then they do not satisfy none of the conditions.', '\n', file=file, append=TRUE) 
    #     Assign <- Easy_case(cnf=cnf, dnf=dnf)
    
    #if(n.c==0) Assign <- unlist(dnf) else Assign <- unlist(cnf)
    Assign <- MinimumConflictAssignment(CNF=cnf, DNF=dnf)
    
    return(Assign)
  }
  #++++++++++++++++++++++++++++
  #       First Condition
  #++++++++++++++++++++++++++++
  
  for (i in 1:n.c)
  {
    for ( j in 1:n.d)
    {
      temp <- intersect(cnf[[i]], dnf[[j]])
      
      # length(temp)==0 means that m_i and c_j have no intersection with each other
      # then first condition is not met and we return False
      if (length(temp) == 0)
      {
        if(verbose) cat('The first condtion is not satisfied because clause ', i, 'and monomial ', j, ' do not have common terms.', '\n' , file=file, append=TRUE) 
        Assign <- Easy_case(cnf=cnf, dnf=dnf)
        return(Assign)
      }
    }
  }
  
  #++++++++++++++++++++++++++++
  #       Second Condition
  #++++++++++++++++++++++++++++
  
  check <- setequal(unlist(cnf), unlist(dnf))
  if (check == F)
  {
    #     browser()
    if(verbose) cat('The second condtion is not satisfied because CNF and DNF have not the same set of variables.', '\n' , file=file, append=TRUE) 
    Assign <- Easy_case(cnf=cnf, dnf=dnf)
    return(Assign)
  }  
  
  #++++++++++++++++++++++++++++
  #       Third Condition
  #++++++++++++++++++++++++++++
  t1 <- max(sapply(cnf, length)) <= n.d # max{|c|: c \in CNF} <= |D|
  t2 <- max(sapply(dnf, length)) <= n.c # max{|m|: m \in DNF} <= |C|
  check <-  t1 & t2
  if (check == F)
  {
    #     browser()
    if(verbose) cat('The third condtion is not satisfied because:', '\n', 'max{|c|: c in CNF} <= |D| is ', t1, '\n',
                    ' and ', '\n',
                    'max{|m|: m in DNF} <= |C| is ', t2, '\n' , file=file, append=TRUE) 
    Assign <- Easy_case(cnf=cnf, dnf=dnf)
    return(Assign)
  }
  
  # If we reach here, it means that all of conditions are satisfied.
  if(verbose) cat('All of conditions are satisfied by CNF and DNF.', '\n', file=file, append=TRUE)
  return(Assign)
}