rm(list=ls())
setwd("C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Code")
cat("\014")

VAR.NUM <- c(8)
GraphGen.method <- 'ThresholdGraph' #'MatchingGraph'

#library(R.matlab)
library(R.matlab)

source("RecursiveCalls_Analysis.R")
source("Hypergraph2IncidenceMat.R")
source("DNFGenerating_FK.R")
source("StandardProblems.R")

# Recursion.counter <<- 0

# CNF.l <- list(c(1,2), c(1,3,4), c(5, 6, 2), c(1,7,8))

standard <- 1 # it indicates that we wanna run the program on standard problems


##################################################

split.methods <- c('random')#, 'lessFreq','mostFreq')#,'adaptive', 'mostFreqSum'
dnf_init.methods <- c('random')#,'combination')#

Save_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Results/"

# plot.file <- paste(Save_path, "DNFlenIteration_lessFreq.pdf", sep='')
# pdf(plot.file)


# NO indicates to the problem which we want to read CNF and DNF files and check their equivalence
NO <- 10

for (NO in VAR.NUM)
{
  cat('Problem: ', NO, '\n')
  if (standard==0)
  {
    
    # split.method <- 'mostFreq'
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
    
    # dnf.dat <- paste(Data_path, "in", NO, '.txt', sep='')
    # DNF <- read.table(dnf.dat, fill=T,
    #                   col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
    #                   header=F, sep = " ")
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Converting CNF and DNF to lists in order to removing NA values
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    library(plyr)
    CNF.l <- alply(CNF, 1)
    names(CNF.l) <- NULL
    
    #+++++++++++++++++++++++++++++++++++++++++++
    # Removing NA values from CNF.l and DNF.l
    #+++++++++++++++++++++++++++++++++++++++++++
    for(i in 1:length(CNF.l))
    {
      ind <- which(!is.na(CNF.l[[i]]))
      CNF.l[[i]] <- CNF.l[[i]][ind]
    }
  }else{
    if (GraphGen.method=='MatchingGraph') CNF.l <- MatchingGraph(NO)
    if (GraphGen.method=='ThresholdGraph') CNF.l <- ThresholdGraph(NO)
    
  }
  all.vars <- sort(unique(unlist(CNF.l)))
  all.vars <- all.vars[!is.na(all.vars)]
  var.num <- length(all.vars)
  
  #++++++++++++++++++++++++++++++++++++++++
  #     Finidng DNF using FK-B algorithm
  #+++++++++++++++++++++++++++++++++++++++++
  dnfinit <- split <- FK.len <- FK.iter <- FK.infiniteCounter <- NULL
  FK.dnfLen <- list()
  k=1
  m=1
  counter <- 1
  for(m in 1:length(dnf_init.methods))
  {
    dnf.init <- dnf_init.methods[m]
    
    for(k in 1:length(split.methods))
    {
      split.method <- split.methods[k]
      
      split[counter] <- split.method
      dnfinit[counter] <- dnf.init
      
      set.seed(k)
      res <- DNFGenerating_FK(cnf=CNF.l, split.method=split.method, infinite.thresh=10, dnf.init=dnf.init)
      
      check <- FK_B(cnf=CNF.l, dnf=res$dnf, verbose=FALSE, split.method)
      if (!is.null(check)) # try again
      {
        set.seed(runif(1))
        res <- DNFGenerating_FK(cnf=CNF.l, split.method=split.method, infinite.thresh=10, dnf.init=dnf.init)
        check <- FK_B(cnf=CNF.l, dnf=res$dnf, verbose=FALSE, split.method)
        if (!is.null(check))
          stop("Check Main_GeneratingDual function!!")
      }
      if(res$Solved==1)
      {
        FK.len[counter] <- length(res$dnf)
        FK.iter[counter] <- res$iteration
        FK.infiniteCounter[counter] <- res$infinite.counter
        FK.dnfLen[[counter]] <- res$dnf.len
        
      }#end if
      counter <- counter+1
    }#end split.method
  }#end dnf_init.method
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #                                                     +
  #             RUNNING OTHER ALGORITHMS                +
  #                                                     +
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++
  path <- 'C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Data/'
  if(standard==0) 
  {
    cnf.dat <-paste(path, "out", NO, ".txt", sep='')
    cnf.dat <- gsub('/', "\\", cnf.dat, fixed=T)
  }else
  {
    cnf.dat <-paste(path, "tempCNF.txt", sep='')
    cnf.dat <- gsub('/', "\\", cnf.dat, fixed=T)
    if (file.exists(cnf.dat)) file.remove(cnf.dat)
    lapply(CNF.l, write, cnf.dat, append=TRUE) # write the list in text file  
  }
  
  
  dnf.dat <- paste(path, "tempfile.txt", sep='')
  dnf.dat <- gsub('/', "\\", dnf.dat, fixed=T)
  
  #++++++++++++++++++++++++++
  #       Running BMR
  #++++++++++++++++++++++++++
  if (file.exists(dnf.dat)) file.remove(dnf.dat)
  BMR.iter <- system(paste("BMR ", cnf.dat, " ", dnf.dat, sep=''), intern = TRUE)[2]
  BMR.iter <- as.numeric(gsub(' = ', '', strsplit(BMR.iter, 'iteration')[[1]][2]))
  # To get DNF length
  BMR.len <- nrow(read.table(dnf.dat, fill=T,
                             col.names = paste0("V",seq_len(max(count.fields(dnf.dat, sep = ' ')))), # This line is useful when the number of elements in the first row is less than the number of elements in the other rows
                             header=F, sep = " "))
  
  #+++++++++++++++++++++++++++
  #       Running DL
  #+++++++++++++++++++++++++++
  if (file.exists(dnf.dat)) file.remove(dnf.dat)
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
  
  check <- file.exists(filename)
  while (!check) # sometimes there is a delay between writing the mat file and reading it; to prevent error I have written this while loop
    check <- file.exists(filename)
  
  dt <- readMat(filename, mat=mat)
  berge.toiter <- dt$totaliter
  berge.maxiter <- dt$maxiter
  berge.len <- nrow(dt$res)
  while (is.null(berge.toiter) | is.null(berge.maxiter)) 
  {
    dt <- readMat(filename, mat=mat)
    berge.toiter <- dt$totaliter
    berge.maxiter <- dt$maxiter
    berge.len <- nrow(dt$res)
  }
  #+++++++++++++++++++++++++++++++++++
  #       Writing in the file
  #++++++++++++++++++++++++++++++++++++
  file <- paste(Save_path, "IterationAnalysis.csv", sep='')
  
  today <- Sys.Date()
  today <- format(today, format="%B %d %Y")
  
  name <- ifelse(standard, paste(GraphGen.method,'_', NO, sep=''), NO)
  dt <- data.frame(Date=today, File=name, Method=c(rep('FK_B', length(FK.len)), 'BMR', "DL", 'Berge'),
                   VarNum=var.num,
                   CNF.length=length(CNF.l),
                   DNF.length=c(FK.len, BMR.len, DL.len, berge.len),
                   SplitMethod=c(split, rep('NA', 3)),
                   RestartNum =c(FK.infiniteCounter, rep('NA', 3)),
                   DNFInitializationMethod = c(dnfinit, rep('NA', 3)),
                   Iterations=c(FK.iter, BMR.iter, DL.iter, paste(berge.toiter,'+',berge.maxiter, sep='')), stringsAsFactors=F)
  
  if (!file.exists(file))
  {
    write.table(dt, file=file, row.names = FALSE, sep=",") 
  }else 
  {
    # writes new data at the end of the file
    #       write.table(dt, file=file, append=T, row.names=F, col.names=F,  sep=",")
    
    # Writes data at the top of the file
    temp.df <- read.table(file=file, sep=",", header=1, stringsAsFactors=F, check.names=F)
    
    ## append in front
    df <- rbind(dt, temp.df, stringsAsFactors=F)
    
    ## write back the whole data frame
    write.table(df, file=file, row.names=F, sep=",")
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #         Plotting number of monomials in DNF obtained by FK_B against iterations
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
#   library(ggplot2)
#   
#   dt <- list()
#   i=1
#   for ( i in 1:length(split))
#     dt[[i]] <- data.frame(SplitVar_Method=split[i],
#                           Initialize_Method=dnfinit[i],
#                           Iteration=(1:length(FK.dnfLen[[i]])-1), 
#                           DNF.Length=FK.dnfLen[[i]])  
#   
#   DT <- do.call(rbind, dt)
#   
#   title <- ifelse(standard, paste(GraphGen.method, '_', NO, sep=''), paste("Problem ", NO, sep=''))
#   
#   P <- ggplot(DT, aes(x=Iteration, y=DNF.Length, color=Initialize_Method, shape=SplitVar_Method), group=interaction(Initialize_Method,SplitVar_Method)) +
#     geom_line(aes(linetype=Initialize_Method), show.legend = T, size=.5) + 
#     geom_point(size=4)+
#     scale_x_continuous(breaks= seq(0,max(DT$Iteration), by=2), limits=c(0, max(DT$Iteration))) +
#     #   scale_x_discrete(limits=1:max(DT$Iteration)) +
#     theme(legend.text=element_text(size=17), 
#           axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
#           text = element_text(size=17), aspect.ratio=1)+ #
#     ylab("Length of DNF")+
#     xlab("Iteration")+
#     ggtitle(title)
#   print(P)
#   
}# end of for (NO)
# dev.off()
