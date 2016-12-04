rm(list=ls())
setwd("C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Code")
cat("\014")

library(ggplot2)

Data_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Results/"
Save_path <- "C:/Users/sedaghat/Dropbox/MONET_MetabolicNetworks/Figures/"
data <- read.csv(file=paste(Data_path, "IterationAnalysis.csv", sep=''), stringsAsFactors=F)

data[,'Method']
data[,'DNFInitializationMethod'] <- substr(data[,'DNFInitializationMethod'],1 , 1)
data[,'SplitMethod'] <- substr(data[,'SplitMethod'],1 , 1)
data[which(data[,'SplitMethod']=='m'), 'SplitMethod'] <- 'mF'
data[which(data[,'SplitMethod']=='l'), 'SplitMethod'] <- 'lF'

data[, 'Method'] <- paste(data[,'Method'], data[,'DNFInitializationMethod'], data[,'SplitMethod'], sep='-')
data[, 'Method'] <- gsub('-NA-NA', '', data[, 'Method'])

data[,'File'] <- paste('P', data[,'File'], ' (', data[,'VarNum'], '-', data[,'CNF.length'],')',sep='')

# removing unused columns
data <- data[, -which(colnames(data) %in% c('Date', 'VarNum', 'DNFInitializationMethod', 'SplitMethod'))]
colnames(data)


# cbPalette <- c("#3399FF", "#CC0000", "#FF9933", "#339900", 
#                "#FF99CC","#009999", "#663399")

library(RColorBrewer)
n <- 11
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
cbPalette <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

data$File <- factor(data$File, levels = c("P1 (5-5)", "P2 (5-4)", "P3 (6-7)", 
                                          "P4 (6-6)", "P5 (6-6)", "P6 (7-8)", "P7 (7-6)", 
                                          "P8 (5-5)", "P9 (9-14)", "P10 (9-15)"))

shapes <- 0:10
plot.file <- paste(Save_path, "Comparison.pdf", sep='')
pdf(plot.file)

#++++++++++++++++++++++++++++++++++++++++
#         Plot number of iterations
#++++++++++++++++++++++++++++++++++++++++

# p1 <- ggplot(data, aes(x=File, y=Iterations, fill=Method)) + 
#   geom_bar(stat="identity",position="dodge") +
#   #   ggtitle(expression('ER'^'+')) +
#   scale_fill_manual(values=cbPalette)+
#   theme(legend.text=element_text(size=15), 
#         axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
#         text = element_text(size=15), aspect.ratio=1)+ #
#   ylab("Iterations")+
#   xlab("Problem")+
#   ggtitle('Comparing number of iterations')

p1 <- ggplot(data, aes(x=File, y=Iterations), group=Method) + 
  geom_line(aes(group=Method, color=Method)) +
  geom_point(aes(color=Method), size=3)+
  #   ggtitle(expression('ER'^'+')) +
  scale_color_manual(values=cbPalette)+
  theme(legend.text=element_text(size=15), 
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size=15), aspect.ratio=1)+ #
  ylab("Iterations")+
  xlab("Problem")+
  ggtitle('Comparing number of iterations')


print(p1)
#++++++++++++++++++++++++++++++++++++++++
#         Plot number of restarts
#++++++++++++++++++++++++++++++++++++++++
dt <- data[which(substr(data[,'Method'],1 ,4)=='FK_B'),]
p2 <- ggplot(dt, aes(x=File, y=RestartNum, fill=Method)) + 
  geom_bar(stat="identity",position="dodge") +
  scale_fill_manual(values=cbPalette)+
  theme(legend.text=element_text(size=15), 
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size=15), aspect.ratio=1)+ #
  ylab("Number of restarts")+
  xlab("Problem")+
  ggtitle('Comparing number of restarts')

print(p2)

dev.off()