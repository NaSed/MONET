rm(list=ls())
setwd("C:\\Users\\sedaghat\\Dropbox\\MONET_Metabolic Networks\\Code")
cat("\014")


Data_path <- "C:/Users/sedaghat/Dropbox/MONET_Metabolic Networks/Data_Result//"
NO <- 4

a <- matrix(c(1, 2, 3, 
              1, 2, 4, 
              1, 5, NA,
              2, 5, NA), ncol=3, byrow=T) 
write.table(a, file=paste(Data_path, "CNF_", NO, '.txt', sep=''), row.names=F, col.names=F)




a <- matrix(c(1, 2, NA,
              3, 4, 5,
              1, 5, NA,
              5, 2, NA), ncol=3, byrow=T) 
write.table(a, file=paste(Data_path, "DNF_", NO, '.txt', sep=''), row.names=F, col.names=F)
