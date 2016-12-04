BMR_method <- function(path, CNF.l)
{
  
  cnf.dat <-paste(path, "tempCNFbmr.txt", sep='')
  cnf.dat <- gsub('/', "\\", cnf.dat, fixed=T)
  if (file.exists(cnf.dat)) file.remove(cnf.dat)
  lapply(CNF.l, write, cnf.dat, append=TRUE) # write the list in text file  
  
  
  dnf.dat <- paste(path, "tempbmr.txt", sep='')
  dnf.dat <- gsub('/', "\\", dnf.dat, fixed=T)
  
  BMR.iter <- system(paste("BMR ", cnf.dat, " ", dnf.dat, sep=''), intern = TRUE)[2]
  BMR.iter <- as.numeric(gsub(' = ', '', strsplit(BMR.iter, 'iteration')[[1]][2]))
  # To get DNF length
  BMR.len <- nrow(read.table(dnf.dat, fill=T,
                             col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                             header=F, sep = " "))
  
  if (file.exists(dnf.dat)) file.remove(dnf.dat)
  return(list(iter=BMR.iter, len=BMR.len))
  
}

###############################################################
DL_method <- function(path, CNF.l)
{
  
  cnf.dat <-paste(path, "tempCNFdl.txt", sep='')
  cnf.dat <- gsub('/', "\\", cnf.dat, fixed=T)
  if (file.exists(cnf.dat)) file.remove(cnf.dat)
  lapply(CNF.l, write, cnf.dat, append=TRUE) # write the list in text file  
  
  
  dnf.dat <- paste(path, "tempdl.txt", sep='')
  dnf.dat <- gsub('/', "\\", dnf.dat, fixed=T)
  
  DL.iter <- system(paste("DL ", cnf.dat, " ", dnf.dat, sep=''), intern = TRUE)[2]
  DL.iter <- as.numeric(gsub(' = ', '', strsplit(DL.iter, 'iteration')[[1]][2]))
  
  DL.len <- nrow(read.table(dnf.dat, fill=T,
                            col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                            header=F, sep = " "))
  
  if (file.exists(dnf.dat)) file.remove(dnf.dat)
  return(list(iter=DL.iter, len=DL.len))
  
}
###############################################################

Berge_method <- function(path, CNF.l, NO)
{
  
  mat <- Hypergraph2IncidenceMat(CNF.l)
  filename <- paste(path, "in" , NO, '.mat', sep='')
  if (file.exists(filename)) file.remove(filename)
  writeMat(filename, mat=mat)
  
  out.filename <- paste("C:\\Users\\sedaghat\\Dropbox\\MONET_MetabolicNetworks\\Data\\BERGEout",NO, '.mat', sep='')
  if (file.exists(out.filename)) file.remove(out.filename)  
  
  system(paste("matlab /minimize /nosplash /nodesktop /r Main('", path, "in", NO, ".mat')", sep=''))
  
    
  check <- file.exists(out.filename)
  while (!check) # sometimes there is a delay between writing the mat file and reading it; to prevent error I have written this while loop
    check <- file.exists(out.filename)
  
  dt <- readMat(out.filename, mat=mat)
  berge.toiter <- dt$totaliter
  berge.maxiter <- dt$maxiter
  berge.len <- nrow(dt$res)
  while (is.null(berge.toiter) | is.null(berge.maxiter)) 
  {
    dt <- readMat(out.filename, mat=mat)
    berge.toiter <- dt$totaliter
    berge.maxiter <- dt$maxiter
    berge.len <- nrow(dt$res)
  }
  return(list(iter=paste(berge.toiter, '+', berge.maxiter, sep=''), len=berge.len))
}