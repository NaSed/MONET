# This function generates DNF based on Matching M(v) method

# Input: v is the number of variables
# output: DNF form
MatchingGraph <- function(v)
{
  
  if (v%%2==1)
    stop("v must be even.")
  
  monomials <- list()
  counter <- 1
  for (i in seq(2, v, by=2))
  {
    monomials[[counter]] <- c(i-1, i)
    counter <- counter+1
  }
  
  return(DNF=monomials)
}

###################################
###################################

ThresholdGraph <- function(v)
{
  if (v%%2==1)
    stop("v must be even.")
  
  monomials <- list()
  counter <- 1  
  for (i in 1:v) 
    for(j in seq(2, v, by=2))
    {
      if (i < j)
      {
        monomials[[counter]] <- c(i,j)
        counter <- counter+1
      }
      
    }
  return(DNF=monomials)
}

####################################
####################################
