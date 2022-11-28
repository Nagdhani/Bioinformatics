# graphs 
# using BLASTCompare and TxHist dataframes from FinalSequenceSort.Rmd 

# setup 
library(ggplot2) 

# density plot of similarity values 
ggplot(BLASTCompare) + geom_density(aes(x = SimilarityPct))

# new version with nice colors 
# https://www.r-bloggers.com/2022/07/how-to-change-background-color-in-ggplot2-3/
ggplot(BLASTCompare) + geom_density(aes(x = SimilarityPct), size = 2) + 
  theme(panel.background = element_rect(fill = '#DAE3F3', color = 'black'),
              panel.grid.major = element_line(color = 'grey', linetype = 'solid'),
              panel.grid.minor = element_line(color = 'grey'),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14)) + 
  xlab("Percent Similarity") + 
  ylab("Density") 

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

# statistics 
mean(PIdata$SimilarityPct) # 98.82379 
mean(NRTIdata$SimilarityPct) # 98.83023 
mean(NNRTIdata$SimilarityPct) # 99.11367 
mean(INIdata$SimilarityPct) # 98.84467

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

# colorful version 
PIplot2 <- ggplot(PIdata) + geom_density(aes(x = SimilarityPct), size = 1) + xlim(95.75, 100.25) + ylim(0,0.7) + 
  theme(panel.background = element_rect(fill = '#DAE3F3', color = 'black'),
        panel.grid.major = element_line(color = 'grey', linetype = 'solid'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) + 
  xlab("Percent Similarity") + 
  ylab("Density")
NRTIplot2 <- ggplot(NRTIdata) + geom_density(aes(x = SimilarityPct), size = 1) + xlim(95.75, 100.25) + ylim(0,0.7)+ 
  theme(panel.background = element_rect(fill = '#DAE3F3', color = 'black'),
        panel.grid.major = element_line(color = 'grey', linetype = 'solid'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+ 
  xlab("Percent Similarity") + 
  ylab("Density")
NNRTIplot2 <- ggplot(NNRTIdata) + geom_density(aes(x = SimilarityPct), size = 1) + xlim(95.75, 100.25) + ylim(0,0.7)+ 
  theme(panel.background = element_rect(fill = '#DAE3F3', color = 'black'),
        panel.grid.major = element_line(color = 'grey', linetype = 'solid'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+ 
  xlab("Percent Similarity") + 
  ylab("Density")
INIplot2 <- ggplot(INIdata) + geom_density(aes(x = SimilarityPct), size = 1) + xlim(95.75, 100.25) + ylim(0,0.7)+ 
  theme(panel.background = element_rect(fill = '#DAE3F3', color = 'black'),
        panel.grid.major = element_line(color = 'grey', linetype = 'solid'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+ 
  xlab("Percent Similarity") + 
  ylab("Density")

# display graphs together 
ggarrange(PIplot2, NRTIplot2, NNRTIplot2, INIplot2, labels = c("PI", "NRTI", "NNRTI", "INI"), ncol = 2, nrow = 2) 


# ---------------- Not working yet ---------------------- 
# bin different mutation rates 
BLASTCompare$PctBinned <- cut(BLASTCompare$SimilarityPct, c(94, 96, 98, 100))

viral <- read.table("ACTG5257_RNA.txt", header = TRUE) 
cd4 <- read.table("ACTG5257_CD4.txt", header = TRUE)

# making new dataframe with combined data (viral load, cd4 count, and mutation rate) so that we can use the dual y axis graph code 


# BLASTCompare uses the patient aliases and the viral and cd4 data use the ID => have to match those up 
ptAlias <- read.table("ACTG5257_Treat_history.txt", sep = "\t", header = TRUE) # load fresh data 
ptAlias <- ptAlias[,1:2] # only take patient ID and alias columns 
ptAlias <- unique(ptAlias) # remove repeats 
ptAlias <- ptAlias[which(ptAlias$Alias %in% BLASTCompare$PatientName),] # remove patients that aren't in BLASTCompare 
BLASTCompare$PatientID <- ptAlias$PtID # add patient ID to BLASTCompare 


# find the bin for each row on viral / cd4 dataframes and then graph each bin separately 
# ie each bin would have its own line graph with two y axes 

viral <- viral[which(viral$PtID %in% BLASTCompare$PatientID),] # remove patients that aren't in BLASTCompare  
cd4 <- cd4[which(cd4$PtID %in% BLASTCompare$PatientID),] # remove patients that aren't in BLASTCompare  

# set up empty columns 
# note for testing: these two lines MUST be run before the for loop each time 
viral$PctBinned <- c(rep("",length(viral$PtID))) 
cd4$PctBinned <- c(rep("",length(cd4$PtID))) 


# add column with bins to viral 
for (x in 1:length(viral$PtID)) {
  bcrow <- which(BLASTCompare$PatientID == viral$PtID[x]) 
  # print(bcrow) 
  # print(BLASTCompare[bcrow,10])
  viral$PctBinned[x] <- BLASTCompare[bcrow, 10] 
  
}
viral$PctBinned <- factor(viral$PctBinned, levels = c(1,2,3), labels = c("< 96", "96-98", "98-100")) 
  
# add column with bins to cd4 
for (x in 1:length(cd4$PtID)) {
  bcrow <- which(BLASTCompare$PatientID == cd4$PtID[x]) 
  # print(bcrow)
  cd4$PctBinned[x] <- BLASTCompare[bcrow, 10]
  
}
cd4$PctBinned <- factor(cd4$PctBinned, levels = c(1,2,3), labels = c("< 96", "96-98", "98-100")) 


# make one dataframe for cd4 + viral load for each bin 
# from viral: date, viral load 
# from cd4: date, CD4Count 
va <- which(viral$PctBinned == '< 96') # rows for combo a (<96) 
ca <- which(cd4$PctBinned == "< 96") 
va
# the values for va are not correct and i have no idea why 


# this is the point at which averages need to be done !!!!!!!!!!!!!!!!!! roughly 


dates <- c(viral$RNADate[va], cd4$CD4Date[ca]) # just get dates 
dates <- unique(dates) # remove duplicates 
# this feels way more complicated than it has to be 

comboa <- data.frame(dates)
values <- c()

# add viral data 
for (x in 1:length(comboa$dates)) {
  if (x %in% va){
    values <-  c(values, viral$VLoad[which(viral$RNADate == comboa$dates[x])])
    print(values)
    # comboa$viralLoad[x] <- 
  }
  
  
} 
#UGHHHHHHHHHHH 

# add cd4 data 
for (x in 1:length(comboa$dates)) {
  comboa$CD4Count[x] <- cd4$CD4Count[which(cd4$CD4Date == comboa$dates[x])] 
  
}

# im getting warnings but im gonna check if the data are correct and if so then just ignore the warnings 
# fuck wait 
# i need to get the average bc there can be multiple patients that have the same viral load on the same day 
# fuckkkkkkk 
# so basically i need to go back through what i have and check if its working mostly correctly and if so i can just add in the averaging somewhere 
  

# the following code is modified from https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html 
# -- 
# Value used to transform the data
coeff <- (sum(cd4$CD4Count) / length(cd4$CD4Count)) / (sum(viral$VLoad) / length(viral$VLoad)) # scale transform value for graph, maybe do 150 for more even axis tick marks 

# --------- <96 % ---------- 
# for patient 260273 /pt227  
# this is the only patient with <96% similarity 
# nothing's working so I'm typing it manually
# removed dates from viral that werent in cd4 
week <- c(-2, 0,  
           23, 34, 49, 
           62, 79, 95, 108, 
           125, 133, 138) 
vload1 <- c(5.1, 5.0, 
           1.6, 1.6, 4.5, 
           1.6, 1.6, 2.0, 2.8, 
           2.9, 5.1, 2.6) 
ccount1 <- c(72, 69, 131, 203, 223, 
            340, 388, 402, 460, 525, 
            418, 476)
data1 <- data.frame(week, vload1, ccount1)

# A few constants
viralColor <- "#69b3a2"
cd4Color <- rgb(0.2, 0.6, 0.9, 1)

plot1 <- ggplot(data1, aes(x=week)) +
  
  geom_line( aes(y=vload1), size=2, color=viralColor) + 
  geom_line( aes(y=ccount1 / coeff), size=2, color=cd4Color) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Viral Load",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="CD4 Count")
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = viralColor, size=13),
    axis.title.y.right = element_text(color = cd4Color, size=13)
  ) +
  
  ggtitle("Patient 227 Outcomes")

# ---------- 96-98% -------- 
# patient number 260070 / pt24 
# didn't look at viral load / cd4 numbers, just picked one that had data for a good number of weeks 


week <- c(0, 1, 4, 15, 
           24, 36, 48, 64, 72, 
           81, 96, 112, 126) 
vload2 <- c(4.6, 4.5, 2.8, 1.6,
            1.6, 1.6, 1.6, 3.8, 1.6,
            1.6, 1.7, 1.6, 1.6) 
ccount2 <- c(370, 300, 446, 409,
             666, 538, 650, 518, 539,
             497, 595, 515, 714)
data2 <- data.frame(week, vload2, ccount2)

# A few constants
viralColor <- "#69b3a2"
cd4Color <- rgb(0.2, 0.6, 0.9, 1)

plot2 <- ggplot(data2, aes(x=week)) +
  
  geom_line( aes(y=vload2), size=2, color=viralColor) + 
  geom_line( aes(y=ccount2 / coeff), size=2, color=cd4Color) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Viral Load",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="CD4 Count")
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = viralColor, size=13),
    axis.title.y.right = element_text(color = cd4Color, size=13)
  ) +
  
  ggtitle("Patient 24 Outcomes")


# ---------- 98-100 ------------- 
# patient number 260066 / pt20 
# didn't look at viral load / cd4 numbers, just picked one that had data for a good number of weeks 


week <- c(0, 1, 5, 17, 25,
           37, 49, 65, 81, 97, 
           114, 129, 149, 167, 181) 
vload3 <- c(4.2, 4.0, 1.6, 1.6, 1.6,
            1.6, 1.6, 1.6, 1.6, 1.6, 
            1.6, 1.6, 1.6, 4.2, 1.6) 
ccount3 <- c(458, 479, 432, 427, 389,
             652, 523, 536, 784, 739,
             802, 849, 914, 600, 1110)
data3 <- data.frame(week, vload3, ccount3)

# A few constants
viralColor <- "#69b3a2"
cd4Color <- rgb(0.2, 0.6, 0.9, 1)

plot3 <- ggplot(data3, aes(x=week)) +
  
  geom_line( aes(y=vload3), size=2, color=viralColor) + 
  geom_line( aes(y=ccount3 / coeff), size=2, color=cd4Color) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Viral Load",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="CD4 Count")
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = viralColor, size=13),
    axis.title.y.right = element_text(color = cd4Color, size=13)
  ) +
  
  ggtitle("Patient 20 Outcomes")


# -- 

# put em all together 
ggarrange(plot1, plot2, plot3, labels = c("", "", ""), ncol = 2, nrow = 2)
 


# --------------ushr stuff-------------------- 
library(ushr)
udata <- data.frame(viral$PtID, viral$VLoad, viral$RNADate)
colnames(udata) <- c("id", "vl", "time")

output <- ushr(udata, filter = FALSE)
# okay idk what to do with this :-/ 
