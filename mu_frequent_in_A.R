# This function checks whether split variable satisfy the below condition:
# |{m \in A: x \in m}|/|A| <= 1/mu(|A| . |B|)
# which we say that x is at most frequent in A
mu_frequent_in_A <- function(split.var, A, B)
{
  # \mu_function is needed for choosing splitting variable
  mu_function <- function(n)    return(log(n)/log(log(n)))
  
  left.side <- sum(sapply(A, function(x) is.element(split.var, x)))/length(A)
  right.side <- 1/mu_function(length(A) * length(B))
  
  return(left.side <= right.side)
  
}