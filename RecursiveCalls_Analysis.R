# This function analysis the result file and counts the number of calls and 
# saves the method of choosing split variable
# NO: indicates to problem number
RecursiveCalls_Analysis <- function(NO, path, Name, Status, var.num, CNF.len, DNF.len, CPUtime)
{
  # path <- "C:/Users/sedaghat/Dropbox/MONET_Metabolic Networks/Results/"
  file.name <- paste(path, "Problem_", NO, "_", split.method, '.txt', sep='')
  txt <- scan(file.name, character(0), sep = "\n") # separate each sentence
  ind <- grep('call', txt)  
  n.Rcall <- length(ind)
  split.method <- txt[1] # the first line is the name of method used for choosing split variable
  date <- txt[2] # the first line is the name of method used for choosing split variable
  
  file <- paste(path, Name, sep='')
  
  dt <- data.frame(Date=date, File=NO, 
                   Method='FK-B',
                   VarNum=var.num,
                   CNF.length=CNF.len,
                   DNF.length=DNF.len,
                   Status=Status,
                   SplitMethod=split.method, 
                   RecursiveCalls=n.Rcall,
                   CPUTime=CPUtime)
  
  if (!file.exists(file))
  {
    write.table(dt, file=file, row.names = FALSE, sep=",") 
  }else write.table(dt, file=file, append=T, row.names=F, col.names=F,  sep=",")
  
}