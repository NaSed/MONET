rm(list=ls())
setwd("C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Code")
cat("\014")

VAR.NUM <- c(4,6,8,10, 12)
GraphGen.method <- 'ThresholdGraph' #'MatchingGraph'

library(R.matlab)

source("RecursiveCalls_Analysis.R")
source("Hypergraph2IncidenceMat.R")
source("DNFGenerating_FK.R")
source("StandardProblems.R")

##################################################

split.methods <- c('random')#, 'lessFreq','mostFreq')#,'adaptive', 'mostFreqSum'
dnf_init.methods <- c('random')#,'combination')#

Save_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Results/"

# NO indicates to the problem which we want to read CNF and DNF files and check their equivalence
NO <- 10

for (NO in VAR.NUM)
{
  cat('Problem: ', NO, '\n')
  #####################################################
  #                                                   #
  #                 Read data                         #
  #                                                   #
  #####################################################
  
  if (GraphGen.method=='MatchingGraph') CNF.l <- MatchingGraph(NO)
  if (GraphGen.method=='ThresholdGraph') CNF.l <- ThresholdGraph(NO)
  
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
  source("OtherMethods.R")
  
  #++++++++++++++++++++++++++
  #       Running BMR
  #++++++++++++++++++++++++++
  bmr <- BMR_method(path, CNF.l)
  
  #+++++++++++++++++++++++++++
  #       Running DL
  #+++++++++++++++++++++++++++
  dl <- DL_method(path, CNF.l)
  
  #+++++++++++++++++++++++++++++++
  #           Running Berge
  #+++++++++++++++++++++++++++++++
  berge <- Berge_method(path, CNF.l, NO)
  
  #+++++++++++++++++++++++++++++++++++
  #       Writing in the file
  #++++++++++++++++++++++++++++++++++++
  file <- paste(Save_path, "IterationAnalysis.csv", sep='')
  
  today <- Sys.Date()
  today <- format(today, format="%B %d %Y")
  
  name <- paste(GraphGen.method,'_', NO, sep='')
  dt <- data.frame(Date=today, File=name, Method=c(rep('FK_B', length(FK.len)), 'BMR', "DL", 'Berge'),
                   VarNum=var.num,
                   CNF.length=length(CNF.l),
                   DNF.length=c(FK.len, bmr$len, dl$len, berge$len),
                   SplitMethod=c(split, rep('NA', 3)),
                   RestartNum =c(FK.infiniteCounter, rep('NA', 3)),
                   DNFInitializationMethod = c(dnfinit, rep('NA', 3)),
                   Iterations=c(FK.iter, bmr$iter, dl$iter, berge$iter), stringsAsFactors=F)
  
  if (!file.exists(file))
  {
    write.table(dt, file=file, row.names = FALSE, sep=",") 
  }else 
  {
    
    # Writes data at the top of the file
    temp.df <- read.table(file=file, sep=",", header=1, stringsAsFactors=F, check.names=F)
    
    ## append in front
    df <- rbind(dt, temp.df, stringsAsFactors=F)
    
    ## write back the whole data frame
    write.table(df, file=file, row.names=F, sep=",")
  }
}# end of for (NO)

dt