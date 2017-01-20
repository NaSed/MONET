# This script reads the outputs of Python model processing function which are stoichiometry 
# matrix as well as indices of irreversible reactions.
# Then calls EFMTool matlab function to calculate EFMs

rm(list=ls())
setwd("C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Code/FluxModeCalculator")
cat("\014")

#################################################################
#
#       Obtaining stoichiometry matrix using Python code
#
#################################################################



as=system(paste('cd C:\\Users\\sedaghat\\Dropbox\\MongooseGUI','\n',
  'py -2.7 -i ModelProcessing.py', '\n',
  , '\n',
  'model = s["AG1"]', '\n',
   '\n',
  
  'quit()', '\n', sep=""))
  

path <- "C:\\Users\\sedaghat\\Dropbox\\MongooseGUI\\"

system(paste('py -2.7 -i ', path, 'ModelProcessing.py', '\n', 
             's=', "shelve.open('", path, 'AG1', "'", ')', '\n',
             "model = s['AG1']", '\n',
             'M = model.Matrix', '\n',
             'm = len(M)', '\n',
             "string  = ''", '\n',
             "string += ';'.join([','.join([str(x) for x in M[i]]) for i in range(m)])", '\n',
             "string += ';'", '\n',
             "f = open('", path, "AG1.txt'",",'w')", '\n',
             'f.write(string)', '\n',
             'f.close()','\n',
              'quit()', '\n',
             sep=''), intern =F)

indent.switch(indent=0)
system(paste("py -2.7 -i C:/Users/sedaghat/Dropbox/MongooseGUI/ModelProcessing.py",
"s = shelve.open('C:/Users/sedaghat/Dropbox/MongooseGUI/AG1')",
"model = s['AG1']",
"M = model.Matrix",
"m = len(M)",
"string  = ''",
"string += ';'.join([','.join([str(x) for x in M[i]]) for i in range(m)])",
"string += ';'",
"f = open('C:/Users/sedaghat/Dropbox/MongooseGUI/AG1.txt','w')",
"f.write(string)",
"f.close()",
"quit()", sep='\n'))


cat('hhj', '\n', "jk")

# path <- 'C:/Users/sedaghat/Dropbox (Personal)/MongooseGUI/'
txt <- scan(paste('myCode.py', sep=''), character(0), sep = ";") # separate each sentence

system(txt)

system(paste('s =', "shelve.open('", path, 'AG1', "'", ')', sep=''))


test <- "abc$def`gh`i\\j"
cat(shQuote(test), "\n")
## Not run: system(paste("echo", shQuote(test)))

irr=model.findIrreversibleReactions()
  m = len(irr)
  string  = str(irr)
  f = open("AG1_irr.txt",'w')
  f.write(string)
  f.close()
  













path <- 'C:/Users/sedaghat/Dropbox (Personal)/MongooseGUI/'
txt <- scan(paste(path, 'AG1.txt', sep=''), character(0), sep = ";") # separate each sentence

mat <- list()
for (i in 1:length(txt))
{
  tmp <- unlist(strsplit(txt[i], ','))
  tmp <- sapply(tmp, function(x) ifelse(x=='0', 0, eval(parse(text=x))))
  mat[[i]] <- tmp
}
stoich.mat <- do.call(rbind, mat)
dim(stoich.mat)

# index of irreversible reactions
irr.ind <- scan(paste(path, 'AG1_irr.txt', sep=''), character(0), sep = ";") # separate each sentence
irr.ind <- as.numeric(unlist(strsplit(substr(irr.ind, 2, nchar(irr.ind)-1),',')))

reversibility <- rep(1, ncol(stoich.mat))
reversibility[irr.ind] <- 0

###############################################################
#
#                 Call EFMTool function in MATLAB
#
###############################################################
library(R.matlab)
writeMat(paste(path, 'AG1.mat', sep=''), stoich=stoich.mat, reversibilities=reversibility)

mnet = CalculateFluxModes(stoich, reversibilities)



txt <- scan('EC6_fm_bin.dat', nlines=100) # separate each sentence
txt <- read.table("fm_bin.dat", skip=0 , nrows=5 )

readLines(con <- file("Unicode.txt"), n=1)
