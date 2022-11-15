# graphs 
# using BLASTCompare and TxHist dataframes from FinalSequenceSort.Rmd 

# setup 
library(ggplot2) 

# density plot of similarity values 
ggplot(BLASTCompare) + geom_density(aes(x = SimilarityPct))

# relationship between similarity and study length 
ggplot(BLASTCompare) + geom_point(aes(x = EndDayNumber, y = SimilarityPct))

# BP length 
ggplot(BLASTCompare) + geom_histogram(aes(x = BPlength))

# BP length and similarity 
ggplot(BLASTCompare) + geom_point(aes(x = BPlength, y = SimilarityPct))

# tx history (type) and similarity pct 
# first, split up by tx type 
PIdata <- BLASTCompare[which(BLASTCompare$PI == 1),]
NRTIdata <- BLASTCompare[which(BLASTCompare$NRTI == 1),]
NNRTIdata <- BLASTCompare[which(BLASTCompare$NNRTI == 1),]
INIdata <- BLASTCompare[which(BLASTCompare$INI == 1),]
# no patients for INSTI   

# libraries for putting ggplots together on one page 
library(gridExtra) 
library(ggpubr)

# make all the graphs 
PIplot <- ggplot(PIdata) + geom_density(aes(x = SimilarityPct)) + xlim(95.75, 100.25) + ylim(0,0.7)
NRTIplot <- ggplot(NRTIdata) + geom_density(aes(x = SimilarityPct)) + xlim(95.75, 100.25) + ylim(0,0.7)
NNRTIplot <- ggplot(NNRTIdata) + geom_density(aes(x = SimilarityPct)) + xlim(95.75, 100.25) + ylim(0,0.7)
INIplot <- ggplot(INIdata) + geom_density(aes(x = SimilarityPct)) + xlim(95.75, 100.25) + ylim(0,0.7)

# display graphs together 
ggarrange(PIplot, NRTIplot, NNRTIplot, INIplot, labels = c("PI", "NRTI", "NNRTI", "INI"), ncol = 2, nrow = 2)



