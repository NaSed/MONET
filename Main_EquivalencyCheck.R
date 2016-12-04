rm(list=ls())
setwd("C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Code")
cat("\014")


library(R.matlab)

source("RecursiveCalls_Analysis.R")
source("Hypergraph2IncidenceMat.R")


source("FK_B.R")

# Problem Numbers
Equivalent <- c(1, 2, 3, 5, 6, 10, 11, 12, 14, 15, 16, 17) # CNF and DNF are not equivalent
NotEquivalent <- c(4) #, 7,8,9, 13 CNF and DNF are not equivalent

split.methods <- c('mostFreqSum')#'random', 'mostFreq', 'adaptive'

Save_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Results/"


# NO indicates to the problem which we want to read CNF and DNF files and check their equivalence
NO <- 17
split.method <- 'mostFreqSum'

num <- 1 # Number of different seed values to repeat (it is applicable when split variable is chosen randomly.)
verbose <- TRUE # printing th whole of process in a file (it is useful for tracing the program)
others.alg <- FALSE # Running the other algorithms in addition to FK_B

for (NO in NotEquivalent)#union(, NotEquivalent
{
  #####################################################
  #                                                   #
  #                 Read data                         #
  #                                                   #
  #####################################################
  
  # CNF <- read.table(paste(Data_path, "CNF_", NO, '.txt', sep=''), fill=T, header=F)
  # DNF <- read.table(paste(Data_path, "DNF_", NO, '.txt', sep=''), fill=T, header=F)
  
  Data_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/"
  cnf.dat <- paste(Data_path, "out", NO, '.txt', sep='')
  CNF <- read.table(cnf.dat, fill=T, 
                    col.names = paste0("V",seq_len(max(count.fields(cnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                    header=F, sep = " ")
  
  dnf.dat <- paste(Data_path, "in", NO, '.txt', sep='')
  DNF <- read.table(dnf.dat, fill=T,
                    col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                    header=F, sep = " ")
  
  all.vars <- sort(unique(c(unlist(DNF), unlist(CNF))))
  all.vars <- all.vars[!is.na(all.vars)]
  var.num <- length(all.vars)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Converting CNF and DNF to lists in order to removing NA values
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  library(plyr)
  CNF.l <- alply(CNF, 1)
  names(CNF.l) <- NULL
  
  DNF.l <- alply(DNF, 1)
  names(DNF.l) <- NULL
  
  #+++++++++++++++++++++++++++++++++++++++++++
  # Removing NA values from CNF.l and DNF.l
  #+++++++++++++++++++++++++++++++++++++++++++
  for(i in 1:length(CNF.l))
  {
    ind <- which(!is.na(CNF.l[[i]]))
    CNF.l[[i]] <- CNF.l[[i]][ind]
  }
  for(i in 1:length(DNF.l))
  {
    ind <- which(!is.na(DNF.l[[i]]))
    DNF.l[[i]] <- DNF.l[[i]][ind]
  }
  
  
  for(split.method in split.methods)
  {
    
    if (split.method=="random") num <- 1
    
    #++++++++++++++++++++++++++++++++++++++++
    #     Calling FK-B function 
    #+++++++++++++++++++++++++++++++++++++++++
    
    today <- Sys.Date()
    today <- format(today, format="%B %d %Y")
    
    if (verbose == TRUE) 
    {
      file <- paste(Save_path, "Problem_", NO, "_", split.method, ".txt", sep='')
      cat(split.method, '\n', file=file)
      cat(today, '\n', file=file, append=TRUE)
    }
    
    res <- matrix(NA, ncol=num, nrow=2)
    for (i in 1:num)
    {
      #       i=78
      set.seed(i)
      ptm <- proc.time()
      (A=FK_B(cnf=CNF.l, dnf=DNF.l, verbose=verbose, split.method= split.method))
      time <- as.vector(proc.time() - ptm)[1:3]
      if(verbose == TRUE) cat('Processing Time (user, system, elapsed): ', paste(time, sep=', '), '\n', file=file, append=TRUE) 
      if (length(A)>0)
      {
        A <- as.numeric(A)
        A <- setdiff(A, 0)
        vals <- rep(0, var.num)
        names(vals) <- 1:var.num
        vals[A] <- 1
        res[1,i] <- CNF_value(CNF.l, vals)
        res[2,i] <- DNF_value(DNF.l, vals)
      }
    }
    
    # file.show(file)
    if (is.element(NO, Equivalent) & length(A)==0)    
      result <- 'DNF and CNF in this problem are equivalent and FK-B algorithm works well!'
    
    if (is.element(NO, Equivalent) & length(A)>0)    
      result <- 'DNF and CNF in this problem are equivalent and FK-B algorithm has some issues!'
    
    if (is.element(NO, NotEquivalent) & length(A)>0)    
      result <- 'DNF and CNF in this problem are NOT equivalent and FK-B algorithm works well!'
    
    if (is.element(NO, NotEquivalent) & length(A)==0)    
      result <- 'DNF and CNF in this problem are NOT equivalent and FK-B algorithm has some issues!'
    
    if (is.element(NO, Equivalent))
      if (length(which(!is.na(res)))==0)
      {
        result <- 'DNF and CNF in this problem are equivalent and FK-B algorithm works well!'
      }else result <- 'DNF and CNF in this problem are equivalent and FK-B algorithm has some issues!'
    
    # which(res[1,] == res[2,])
    
    if (is.element(NO, NotEquivalent))
      if (all(res[1,] == !res[2,]))
      {
        result <- 'DNF and CNF in this problem are NOT equivalent and FK-B algorithm works well!'
      }else result <- 'DNF and CNF in this problem are NOT equivalent and FK-B algorithm has some issues!'
    
    cat(result, '\n')
    
    RecursiveCalls_Analysis(NO, path=Save_path, Name="RecursiveCallsAnalysis.csv", 
                            Status=ifelse(is.element(NO, Equivalent), "Equivalent", "NotEquivalent"),
                            var.num=var.num,
                            CNF.len= length(CNF.l), 
                            DNF.len= length(DNF.l),
                            CPUtime=time[2])
  } # split.methods
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                     +
  #             RUNNING OTHER ALGORITHMS                +
  #                                                     +
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (others.alg)
  {
    path <- 'C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/'
    cnf.dat <- paste(path, "out", NO, ".txt", sep='')
    cnf.dat <- gsub('/', "\\", cnf.dat, fixed=T)
    
    dnf.dat <- paste(path, "tempfile.txt", sep='')
    dnf.dat <- gsub('/', "\\", dnf.dat, fixed=T)
    
    #++++++++++++++++++++++++++
    #       Running BMR
    #++++++++++++++++++++++++++
    BMR.iter <- system(paste("BMR ", cnf.dat, " ", dnf.dat, sep=''), intern = TRUE)[2]
    BMR.iter <- as.numeric(gsub(' = ', '', strsplit(BMR.iter, 'iteration')[[1]][2]))
    # To get DNF length
    BMR.len <- nrow(read.table(dnf.dat, fill=T,
                               col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                               header=F, sep = " "))
    
    #+++++++++++++++++++++++++++
    #       Running DL
    #+++++++++++++++++++++++++++
    DL.iter <- system(paste("DL ", cnf.dat, " ", dnf.dat, sep=''), intern = TRUE)[2]
    DL.iter <- as.numeric(gsub(' = ', '', strsplit(DL.iter, 'iteration')[[1]][2]))
    
    DL.len <- nrow(read.table(dnf.dat, fill=T,
                              col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                              header=F, sep = " "))
    
    #mtminer.res <- system(paste("mtminer ", cnf.dat, sep=''), intern = TRUE)
    # add <- 'C:\\Users\\sedaghat\\Dropbox\\MONET_MetabolicNetworks\\Code\\BEGK\\data1.txt'
    # r <- read.table(add, sep=' ', fill=NA)
    
    #+++++++++++++++++++++++++++++++
    #           Running Berge
    #+++++++++++++++++++++++++++++++
    
    mat <- Hypergraph2IncidenceMat(CNF.l)
    filename <- paste(path, "in",NO, '.mat', sep='')
    writeMat(filename, mat=mat)
    system(paste("matlab /minimize /nosplash /nodesktop /r Main('", path, "in", NO, ".mat')", sep=''))
    filename <- paste("C:\\Users\\sedaghat\\Dropbox\\MONET_MetabolicNetworks\\Data\\BERGEout",NO, '.mat', sep='')
    
    while (!file.exists(filename)) # sometimes there is a delay between writing the mat file and reading it; to prevent error I have written this while loop
      cheack <- TRUE
    
    dt <- readMat(filename, mat=mat)
    berge.iter <- dt$iter
    berge.len <- nrow(dt$res)
    #+++++++++++++++++++++++++++++++++++
    #       Writing in the file
    #++++++++++++++++++++++++++++++++++++
    file <- paste(Save_path, "RecursiveCallsAnalysis.csv", sep='')
    
    dt <- data.frame(Date=today, File=NO, Method=c('BMR', "DL", 'Berge'),
                     VarNum=var.num,
                     CNF.length=length(CNF.l),
                     DNF.length=c(BMR.len, DL.len, berge.len),
                     Status=ifelse(is.element(NO, Equivalent), "Equivalent", "NotEquivalent"),
                     SplitMethod='NA', 
                     RecursiveCalls=c(BMR.iter, DL.iter, berge.iter),
                     CPUTime='NA')
    
    if (!file.exists(file))
    {
      write.table(dt, file=file, row.names = FALSE, sep=",") 
    }else write.table(dt, file=file, append=T, row.names=F, col.names=F,  sep=",")
  }# Running Other algorithms
}#NO

