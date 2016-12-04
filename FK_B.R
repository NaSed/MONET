# The below code has been written based on the FK-B algorithm presented in "how to apply sat-solving for the equivalence test of mootone normal forms", page 5
FK_B <- function(cnf, dnf, verbose=FALSE, split.method)
{
  
  source("MinimumConflictAssignment.R")
  source("mu_frequent_in_A.R")
  source("A_m_x.R")
  source("DNF_value.R")
  source("CNF_value.R")
  source("A_c_x.R")
  source("phi_x_1.R")
  source("phi_x_0.R")
  source("Easy_case.R")
  source("Check_Conditions.R")
  source("Irredundant.R")
  source("Choose_SplitVar.R")
  
#   Recursion.counter <<- Recursion.counter +1
  ############################################################
  Assign <- list()
  #   if(length(cnf)==2)
  #     if (all(sort(cnf[[1]])==c(6,8,15)) & all(sort(cnf[[2]])== c(6,8,10)))
#   browser()
  if(verbose)
  {
    cat('\n','                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                   ', '\n', file=file, append=TRUE)
    cat('\n','XXXXXXXXXXXXXXXXXXXX    Start of FK-B algorithm     XXXXXXXXXXXXXXXXXXX', '\n', file=file, append=TRUE)
    cat('\n','                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                   ', '\n', file=file, append=TRUE)
    cat('CNF= ', paste('(', sapply(cnf, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
    cat('DNF= ', paste('(', sapply(dnf, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
  }
  #   browser()
  
  ###########################################################
  #                                                         #
  #                     Checking redundancy                 #
  #                                                         #
  ###########################################################  
  #   if( length(cnf)==8 & length(dnf)==2)
  #   {
  #     browser()
  #   }
  if(length(cnf)>0) cnf <- Irredundant(cnf)
  if(length(dnf)>0) dnf <- Irredundant(dnf)
  
  n.C <- length(cnf) # Number of clauses in CNF
  n.D <- length(dnf) # Number of monomials in DNF
  
  if(verbose)
  {  
    cat('\n', 'After removing redundancy:', '\n', file=file, append=TRUE)
    cat('CNF= ', paste('(', sapply(cnf, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
    cat('DNF= ', paste('(', sapply(dnf, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
  } 
  ###########################################################
  #                                                         #
  #                     Checking conditions                 #
  #                                                         #
  ###########################################################
  #   browser()
  Assign <- Check_Conditions(cnf=cnf, dnf=dnf, verbose=verbose)
  if(verbose)
    cat('Assign= ', Assign, '\n', file=file, append=TRUE)
  if (length(Assign)>0)
    return(Assign)
  
  ###########################################################
  #                                                         #
  #                 Trivial Check                           #
  #                                                         #  
  ###########################################################
  
  if (min(n.D, n.C) <= 2)
  {    
#     browser()
    val <- Easy_case(cnf=cnf, dnf=dnf)
    if(verbose)
      cat('\n', 'Easy Case: Equivalence(CNF, DNF)=', val, '\n', file=file, append=TRUE)
    
    if (length(val)==1)
      if (val=='TRUE') # If val=1, then R thinks that it is equal to TRUE, so in 'Easy_check' I return 'TRUE' as a string if they are equivalent
        return(NULL)
    
    return(val)
    
  }else #line 5
  {
#     browser()
    split.var <- switch(split.method,
                        random = sample(unique(unlist(cnf)), 1),
                        mostFreq = Choose_SplitVar(cnf=cnf, dnf=dnf, method='mostFreq'),
                        lessFreq = Choose_SplitVar(cnf=cnf, dnf=dnf, method='lessFreq'),
                        mostFreqSum = Choose_SplitVar(cnf=cnf, dnf=dnf, method='mostFreqSum'),
                        adaptive = Choose_SplitVar(cnf=cnf, dnf=dnf, method='adaptive')
    )
    
    #     split.var <- Choose_SplitVar(cnf=cnf, dnf=dnf)
    if(verbose)
      cat('\n', 'split.var = ', split.var, '\n', file=file, append=TRUE)
    
    if (is.null(cnf) | is.null(dnf)) browser()
    if (length(cnf)==0 | length(dnf)==0) browser()
    
#     browser()
    D_x_1 <- phi_x_1(dnf, split.var)
    D_x_0 <- phi_x_0(dnf, split.var)
    C_x_1 <- phi_x_1(cnf, split.var)
    C_x_0 <- phi_x_0(cnf, split.var)
    
    if(verbose)
    {
      cat('D_x1= ', paste('(', sapply(D_x_1, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
      cat('D_x0= ', paste('(', sapply(D_x_0, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
      cat('C_x1= ', paste('(', sapply(C_x_1, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
      cat('C_x0= ', paste('(', sapply(C_x_0, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
    }
    if (mu_frequent_in_A(split.var, A=dnf, B=cnf))# if split variable is at most mu-frequent in DNF (line 7)
    {
      if(verbose)
      {
        cat('######', '\n', file=file, append=TRUE)
        cat('call line 8: ', 'FK_B(dnf=D_x_1, cnf=c(C_x_0, C_x_1))', '\n', file=file, append=TRUE)
        cat('######', '\n', file=file, append=TRUE)
      }
      
      
      Assign <- FK_B(dnf=D_x_1, cnf=c(C_x_0, C_x_1), verbose=verbose, split.method) # line 8
      if(verbose)
        cat('Assign= ', Assign, '\n', file=file, append=TRUE)
      
      if (length(Assign)>0) return(Assign) #line 9
      for (i in 1: length(C_x_0)) #line 10
      {
        #         browser()
        vars <- C_x_0[[i]]
        
        # Sometimes split.var causes that some of D_x_1, D_x_0, C_x_1, C_x_0 be NULL. look at the below problem:
        # CNF = (2|3|6) ^ (1|2|3) ^ (2|4|6) ^ (1|2|4)  ; DNF=(3^4) | (1^6) | (2); split.var=2
        # In this case, the statements will be TRUE, then they are equivalent
        #         if(length(D_x_0)==0 & length(C_x_1)==0)
        #         {  
        #           browser()
        #           Assign=NULL
        #           if(verbose)
        #             cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        #           
        #         }else{
        #           if(length(D_x_0)==0 | length(C_x_1)==0) browser()
        
        if(length(D_x_0)>0) D_cx_0 <- A_c_x(D_x_0, vars, 'DNF') else D_cx_0 <- D_x_0
        if(length(C_x_1)>0) C_cx_1 <- A_c_x(C_x_1, vars, 'CNF') else C_cx_1 <- C_x_1
        
        if(verbose)
        {
          cat('D_cx_0= ', paste('(', sapply(D_cx_0, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
          cat('C_cx_1= ', paste('(', sapply(C_cx_1, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
          
          cat('######', '\n', file=file, append=TRUE)
          cat('call line 11: ', 'FK_B(dnf=D_cx_0, cnf=C_cx_1)', '\n', file=file, append=TRUE)
          cat('######', '\n', file=file, append=TRUE)
        }
        if(length(D_cx_0)==0 & length(C_cx_1)==0)
        {
          Assign <- NULL
        }else    Assign <- FK_B(dnf=D_cx_0, cnf=C_cx_1, verbose=verbose, split.method) #line 11
        if(verbose)
          cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        #         }
        if (length(Assign)>0)  
        {
          Assign <- c(Assign, split.var) # adding split.var
          if(verbose)
            cat('Assign= ', Assign, '\n', file=file, append=TRUE)
          return(Assign) #line 12
        }
      }#end of for (i in 1: length(C_x_0))      
    }else if(mu_frequent_in_A(split.var, A=cnf, B=dnf))# if split variable is at most mu-frequent in CNF(line 13)
    {
      if(verbose)
      {
        cat('######', '\n', file=file, append=TRUE)
        cat('call line 14: ', 'FK_B(dnf=c(D_x_0, D_x_1), cnf=C_x_1)', '\n', file=file, append=TRUE)
        cat('######', '\n', file=file, append=TRUE)
      }
      Assign <- FK_B(dnf=c(D_x_0, D_x_1), cnf=C_x_1, verbose=verbose, split.method) #line 14
      if(verbose)
        cat('Assign= ', Assign, '\n', file=file, append=TRUE)
      if (length(Assign)>0)  
      {
        Assign <- c(Assign, split.var)
        if(verbose)
          cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        return(Assign) #line 15
      }
      for (i in 1: length(D_x_0)) #line 16
      {
        vars <- unlist(D_x_0[[i]])
        
        # Sometimes split.var causes that some of D_x_1, D_x_0, C_x_1, C_x_0 be NULL. look at the below problem:
        # CNF = (5|6) ^ (2|4) ^ (2|5) ^ (3)  ; DNF=(3^2^6) | (3^4^5) | (5^3^2); split.var=3
        # In this case, the statements will be TRUE, then they are equivalent
        #         if(length(D_x_1)==0 & length(C_x_0)==0)
        #         {  
        #           #           browser()
        #           Assign <- NULL # Assign is NULL because C_x_0 and D_x_1 are TRUE, then D_mx_1 and C_mx_0 are equivalent and there is no conflict set
        #           if(verbose)
        #             cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        #         }
        #         else{
        if(length(D_x_1)>0) D_mx_1 <- A_m_x(D_x_1, vars, 'DNF') else D_mx_1 <- D_x_1
        if(length(C_x_0)>0) C_mx_0 <- A_m_x(C_x_0, vars, 'CNF') else C_mx_0 <- C_x_0
        
        if(verbose)
        {
          cat('D_mx_1= ', paste('(', sapply(D_mx_1, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
          cat('C_mx_0= ', paste('(', sapply(C_mx_0, function(x) paste( x, sep=' ', collapse=',')), ')', sep=''), '\n', file=file, append=TRUE)
          
          cat('######', '\n', file=file, append=TRUE)
          cat('call line 17: ','FK_B(dnf=D_mx_1, cnf=C_mx_0)', '\n', file=file, append=TRUE)
          cat('######', '\n', file=file, append=TRUE)
        }
        
        if(length(D_mx_1)==0 & length(C_mx_0)==0)
        {
          Assign <- NULL
        }else Assign <- FK_B(dnf=D_mx_1, cnf=C_mx_0, verbose=verbose, split.method) #line 17
        if(verbose)
          cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        #         }        
        if (length(Assign)>0)  
        {
          #           browser()
          return(c(Assign, vars)) #line 18 (Original code according to algorithm in the paper)
        }
      }#end of for (i in 1: length(C_x_0))
    }else # line 19
    { 
      if(verbose)
      {
        cat('\n', '######', '\n', file=file, append=TRUE)
        cat('call line 20: ', 'FK_B(dnf=D_x_1, cnf=c(C_x_0, C_x_1)', '\n', file=file, append=TRUE)
        cat('######', '\n', file=file, append=TRUE)
      }
      Assign <- FK_B(dnf=D_x_1, cnf=c(C_x_0, C_x_1), verbose=verbose, split.method)  #line 20
      if(verbose)
        cat('Assign= ', Assign, '\n', file=file, append=TRUE)
      if(length(Assign)==0) # line 21
      {
        #         browser()
        if(verbose)
        {
          cat('######', '\n', file=file, append=TRUE)
          cat('call line 22: ', 'FK_B(dnf=c(D_x_0, D_x_1), cnf=C_x_1)', '\n', file=file, append=TRUE)
          cat('######', '\n', file=file, append=TRUE)
        }
        Assign <- FK_B(dnf=c(D_x_0, D_x_1), cnf=C_x_1, verbose=verbose, split.method) # line 22
        if(verbose)
          cat('Assign= ', Assign, '\n', file=file, append=TRUE)
        
        if (length(Assign)>0)  
        {
          #           browser()
          Assign <- c(Assign, split.var) # split.var
          if(verbose)
            cat('Assign= ', Assign, '\n', file=file, append=TRUE)
          return(Assign) #line 23
        }
        
      }# end if if(length(Assign)==0)
    }# end else if(mu_frequent_in_A(split.var, A=cnf, B=dnf))
  }# end else #line 5
  return(Assign)
  if(verbose)
    cat('Assign= ', Assign, '\n', file=file, append=TRUE)
}
