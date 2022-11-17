###The dopamine hypothesis is outdated: A meta-analysis of the complexities in the biochemistry of schizophrenia; SCRIPT###
###Author: Whitney Domico###


##DETAILED INSTRUCTIONS MAY BE FOUND AT https://github.com/domicowe/BIO247/tree/master/BIO247README UNDER "PROJECT_INSTRUCTIONS"

##importing data
##note: whatever order the articles are in here, they must be consistently in this order when pulling from columns

library(readxl)
PMC3077530_Data <- read.csv("PMC3077530_Data.txt", sep="", header = TRUE)
PMC2775422_Data <- read.csv("PMC2775422_Data.txt", sep="", header = TRUE)
PMC3912837_Data <- read_excel("PMC3912837_Data.xlsx",sheet = "Sheet2")
PMC2890845_Data <- read_excel("PMC2890845_Data.xlsx")
PMC4724864_Data <- read_excel("PMC4724864_Data.xlsx", skip = 1)
PMC6927206_Data <- read_excel("PMC6927206_Data.xlsx", skip = 1)
PMC3872086_Data <- read_excel("PMC3872086_Data.xlsx", skip = 5)
PMC4059435_Data <- read_excel("PMC4059435_Data.xlsx")
PMC3905728_Data <- read_excel("PMC3905728_Data.xlsx", sheet = "Sheet1")
PMC3827979_Data <- read_excel("PMC3827979_Data.xlsx")



##data cleaning
##removing letters from end of PMC3077530$SNP data and combining with other data

temp <- substr(PMC3077530_Data$SNP,1,nchar(PMC3077530_Data$SNP)-3)

temp3 <- c()
for (each in PMC3912837_Data$SNP){
  temp2 <- unlist(strsplit(each, ""))
  if ("*" %in% temp2){
    temp3 <- c(temp3, (substr(each, 2, nchar(each))))
  } else
    temp3 <- c(temp3, each)
}
SNP <- c(temp, PMC2775422_Data$SNP, temp3, PMC2890845_Data$`Tag SNP ID`, PMC4724864_Data$SNP, PMC6927206_Data$SNP...1, PMC3872086_Data$SNP, PMC4059435_Data$SNP, PMC3905728_Data$`Strongest SNP`, PMC3827979_Data$`rs ID`)



##splitting PMC2890845 and PMC3827979 into Chr and position since they were given in same column

temp4 <- c()
for (each in PMC2890845_Data$CNVR) {
  temp <- unlist(strsplit(each, ""))
  temp2 <- c(temp[4:5])
  if (":" %in% temp2 && "X" %in% temp2 == FALSE && "Y" %in% temp2 == FALSE && "x" %in% temp2 == FALSE && "y" %in% temp2 == FALSE){
    temp3 <- paste0(0, temp2[1])
    temp4 <- c(temp4, temp3)
  } else if (":" %in% temp2){
    temp4 <- c(temp4, temp2[1])
  } else
    temp5 <- paste0(temp2[1], temp2[2])
  temp4 <- c(temp4, temp5)
  temp <- c()
  temp2 <- c()
  temp3 <- c()
  temp5 <- c()
}
PMC2890845_Data$Chr <- temp4

temp4 <- c()
for (each in PMC2890845_Data$CNVR) {
  temp <- unlist(strsplit(each, ":"))
  temp2 <- c(temp[2])
  temp3 <- unlist(strsplit(temp2, "–"))
  temp4 <- c(temp4, as.numeric(temp3[1]))
  temp <- c()
  temp2 <- c()
  temp3 <- c()
}
PMC2890845_Data$Position.bp <- temp4

temp4 <- c()
for (each in PMC3827979_Data$ChrPos) {
  temp <- unlist(strsplit(each, ""))
  temp2 <- c(temp[4:5])
  if (":" %in% temp2 && "X" %in% temp2 == FALSE && "Y" %in% temp2 == FALSE && "x" %in% temp2 == FALSE && "y" %in% temp2 == FALSE){
    temp3 <- paste0(0, temp2[1])
    temp4 <- c(temp4, temp3)
  } else if (":" %in% temp2){
    temp4 <- c(temp4, temp2[1])
  } else
    temp5 <- paste0(temp2[1], temp2[2])
  temp4 <- c(temp4, temp5)
  temp <- c()
  temp2 <- c()
  temp3 <- c()
  temp5 <- c()
}
PMC3827979_Data$Chr <- temp4





##putting positions into equal units (millions of base pairs)


temp <- PMC2775422_Data$Position.bp.
temp <- temp/1000000
temp2 <- PMC3912837_Data$Position.bp/1000000
temp3 <- PMC2890845_Data$Position.bp/1000000
temp5 <- PMC4724864_Data$BP/1000000
temp6 <- PMC6927206_Data$BP...3/1000000
temp7 <- PMC4059435_Data$`Location (bp)`/1000000
temp8 <- as.numeric(PMC3905728_Data$`Position (hg18)`)/1000000
temp9 <- PMC3827979_Data$bp/1000000
Position.Mb <- c(PMC3077530_Data$Position.Mb., temp, temp2, temp3, temp5, temp6, PMC3872086_Data$Mb, temp7, temp8, temp9)



##removing junk characters from Chr for PMC3912837; redefining 1 as 01, etc in PMC3077530 and defining Chr

temp4 <- c()
for (each in PMC3912837_Data$Chr) {
  temp <- unlist(strsplit(each, ""))
  temp2 <- c(temp[4:5])
  if (":" %in% temp2 && "X" %in% temp2 == FALSE && "Y" %in% temp2 == FALSE && "x" %in% temp2 == FALSE && "y" %in% temp2 == FALSE){
    temp3 <- paste0(0, temp2[1])
    temp4 <- c(temp4, temp3)
  } else if (":" %in% temp2){
    temp4 <- c(temp4, temp2[1])
  } else
    temp5 <- paste0(temp2[1], temp2[2])
  temp4 <- c(temp4, temp5)
  temp <- c()
  temp2 <- c()
  temp3 <- c()
  temp5 <- c()
}


Chr <- c()
temp6 <- c(PMC3077530_Data$Chr, PMC2775422_Data$Chr, temp4, PMC2890845_Data$Chr, PMC4724864_Data$CHR, PMC6927206_Data$CHR, PMC3872086_Data$Chr, PMC4059435_Data$Chr., PMC3905728_Data$Chromosome, PMC3827979_Data$Chr)
for (each in temp6){
  temp7 <- unlist(strsplit(each, ""))
  if (length(temp7) == 1 && "X" %in% temp7 == FALSE && "Y" %in% temp7 == FALSE && "x" %in% temp7 == FALSE && "y" %in% temp7 == FALSE) {
    temp8 <- paste0(0, each)
    Chr <- c(Chr, temp8)
  } else 
    Chr <- c(Chr, each)
  temp6 <- c()
}



##inserting PMCID columns for all data; defining PMCID

PMC2890845_Data$PMCID <- c("PMC2890845")
PMC3912837_Data$PMCID <- c("PMC3912837")
PMC2775422_Data$PMCID <- c("PMC2775422")
PMC3077530_Data$PMCID <- c("PMC3077530")
PMC4724864_Data$PMCID <- c("PMC4724864")
PMC6927206_Data$PMCID <- c("PMC6927206")
PMC3872086_Data$PMCID <- c("PMC3872086")
PMC4059435_Data$PMCID <- c("PMC4059435")
PMC3905728_Data$PMCID <- c("PMC3905728")
PMC3827979_Data$PMCID <- c("PMC3827979")

PMCID <- c(PMC3077530_Data$PMCID, PMC2775422_Data$PMCID, PMC3912837_Data$PMCID, PMC2890845_Data$PMCID, PMC4724864_Data$PMCID, PMC6927206_Data$PMCID, PMC3872086_Data$PMCID, PMC4059435_Data$PMCID, PMC3905728_Data$PMCID, PMC3827979_Data$PMCID)



##beginning new dataframe for all data

InitialData <- data.frame(SNP, Chr, Position.Mb, PMCID)



##making dataframe with only unique values of SNP (Modified from Stack Exchange question: "Combining Rows into List on R")

library(data.table)
require(data.table)


tempdf2 <- setDT(InitialData)[,.(Chr=paste(Chr,collapse = ",")), by=SNP]

temp2 <- c()
for (each in tempdf2$Chr){
  temp <- unlist(strsplit(each, ","))
  temp2 <- c(temp2, temp[1])
}

tempdf3 <- setDT(InitialData)[,.(Position.Mb=paste(Position.Mb,collapse = ",")), by=SNP]

temp3 <- c()
for (each in tempdf3$Position.Mb){
  temp <- unlist(strsplit(each, ","))
  temp3 <- c(temp3, temp[1])
}

tempdf4 <- setDT(InitialData)[,.(PMCID=paste(PMCID,collapse = ",")), by=SNP]

MainData <- data.frame("SNP"= tempdf2$SNP, "Chr"=temp2, "Position.Mb"=temp3, "PMCID"=tempdf4$PMCID)



##making relative frequency graph (adjusting for differing lengths of chromosomes)


library(readxl)
library(dplyr)
library(ggplot2)
ChrData <- read_excel("ChrData.xlsx")


ObsData <- as.data.frame(table(MainData$Chr))

ObsData$ChrBP <- ChrData$`Length (bp)`
ObsData$Chr <- c(ObsData$Var1[1:23], "Y")

ObsData$AdjObs <- 10^7*(ObsData$Freq/ObsData$ChrBP)

mean <- mean(ObsData$AdjObs)
sd <- sd(ObsData$AdjObs)
mean1 <- rep(1, 24)
onesd <- rep(1+sd,24)
twosd <- rep(1+2*sd, 24)

plot.default(x=ObsData$Var1, y=ObsData$AdjObs, xlab= "Chromosome", ylab=("Relative Frequency of SNP (AU)"), pch=19, col='black', type='p')
lines(x=ObsData$Var1, y=mean1, lty=1)
lines(x=ObsData$Var1, y=onesd, lty=2)
lines(x=ObsData$Var1, y=twosd, lty=3)
legend('topright', c("Population Mean", "One SD Above", "Two SD Above"), lty=c(1,2,3))

ObsData$AdjObs.binned <- cut(ObsData$AdjObs, c(0, 0.5*sd, 1, 1.5*sd, 2.0*sd, 3.0*sd, 3.5*sd, 4*sd, 4.5*sd, 5*sd))


library(ggplot2)
ggplot(ObsData, color= station, fill='black', aes(x=Var1, y=AdjObs))+
  geom_col(position='dodge')+
  ylab("Relative Frequency (AU)")+
  xlab("Chromosome")



##Stat testing for adjusted frequency, keeping chromosomes 2sd+ above mean (which should be 1 for relative frequency)

mean <- mean(ObsData$AdjObs)
sd <- sd(ObsData$AdjObs)

onesd <- 1+sd
twosd <- 1+2*sd

over68 <- c()
over95 <- c()


row <- c(1:length(ObsData$Var1))

for (each in row){
  if (ObsData[each,'AdjObs']> onesd && ObsData[each,'AdjObs'] < twosd){
    over68 <- c(over68, ObsData[each,'Var1'])
  } else if (ObsData[each,'AdjObs'] >= twosd){
    over95 <- c(over95, ObsData[each,'Var1'] )
  }
}
print(over68)
print(over95)

##The chromosomes in vector 'over68' and 'over95' are relevant chromosomes, and you should use these while continuing. The relevant ones in the original dataset were 6, 11, and 22.

##statistical testing for chosen chrs

##Chr6

##cleaning data from NCBI Chr 6 Homology

Chr6DataNCBI <- read_excel("Chr6DataNCBI.xlsx")
Chr6DataNCBI <- Chr6DataNCBI[,1:3]

temp <- c()
generang <- c()
for (each in Chr6DataNCBI$`human position`){
  temp <- unlist(strsplit(each, ':'))
  temp2 <- temp[2]
  temp3 <- unlist(strsplit(temp2, " "))
  temp4 <- temp3[1]
  generang <- c(generang, temp4)
}

Chr6DataNCBI$gene.range <- generang

startpos <- c()
endpos <- c()
for (each in Chr6DataNCBI$gene.range){
  temp <- unlist(strsplit(each, "-"))
  temp2 <- gsub(",", "", temp)
  startpos <- c(startpos, as.numeric(temp2[1])/1000000)
  endpos <- c(endpos, as.numeric(temp2[2])/1000000)
}

Chr6DataNCBI$startpos <- startpos
Chr6DataNCBI$endpos <- endpos



##finding most common SNP range

row <- c(1:length(MainData$SNP))
chr6 <- c()
for (each in row){
  if (MainData[each,2]=="06"){
    chr6 <- c(chr6, as.numeric(MainData[each,3]))
  }
}

library(ggplot2)
testdf <- data.frame("initial"= chr6)
testdf$binnedchr6 <- cut(chr6, c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170))
ggplot(testdf)+geom_bar(aes(x=binnedchr6))


row <- c(1:length(MainData$SNP))
chr6 <- c()
SNP6 <- c()
for (each in row){
  if (MainData[each,2]=="06"){
    if (MainData[each,3]>20 && MainData[each,3]<=40){
      chr6 <- c(chr6, as.numeric(MainData[each,3]))
      SNP6 <- c(SNP6, MainData[each, 'SNP'])
    }
  }
}
data.frame("SNP"=SNP6,"Position.bp"=chr6)

freqtable6 <- as.data.frame(table(testdf$binnedchr6))
mean <- mean(freqtable6$Freq)
sd <- sd(freqtable6$Freq)

onesd <- 1+sd
twosd <- 1+2*sd

over68 <- c()
over95 <- c()

##cut down df to only include significant values

row <- c(1:length(freqtable6$Freq))

for (each in row){
  if (freqtable6[each,'Freq']> onesd && freqtable6[each,'Freq'] < twosd){
    over68 <- c(over68, freqtable6[each,'Var1'])
  } else if (freqtable6[each,'Freq'] >= twosd){
    over95 <- c(over95, freqtable6[each,'Var1'] )
  }
}


##finding genes in Chr6 that include the bp with SNP

potSNP6 <- c()
potgenes6 <- c()
row <- c(1:length(Chr6DataNCBI$startpos))
for (each in chr6){
  for (each2 in row){
    if (each >= Chr6DataNCBI[each2,'startpos'] && each <= Chr6DataNCBI[each2,'endpos']){
      potSNP6 <- c(potSNP6, each)
      potgenes6 <- c(potgenes6, Chr6DataNCBI[each2,3])
    }
  }
}
unqpotgenes6<-unique(potgenes6)

Chr6Genes <- data.frame(potSNP6)
Chr6Genes$potgenes6 <- potgenes6



##cleaning data from NCBI Chr 11 Homology 

Chr11DataNCBI <- read_excel("Chr11DataNCBI.xlsx")
Chr11DataNCBI <- Chr11DataNCBI[,1:3]

temp <- c()
generang <- c()
for (each in Chr11DataNCBI$`human position`){ 
  temp <- unlist(strsplit(each, ':'))
  temp2 <- temp[2]
  temp3 <- unlist(strsplit(temp2, " "))
  temp4 <- temp3[1]
  generang <- c(generang, temp4)
}

Chr11DataNCBI$gene.range <- generang

startpos <- c()
endpos <- c()
for (each in Chr11DataNCBI$gene.range){
  temp <- unlist(strsplit(each, "-"))
  temp2 <- gsub(",", "", temp) 
  startpos <- c(startpos, as.numeric(temp2[1])/1000000)
  endpos <- c(endpos, as.numeric(temp2[2])/1000000)
}

Chr11DataNCBI$startpos <- startpos
Chr11DataNCBI$endpos <- endpos



##finding most common SNP range

row <- c(1:length(MainData$SNP))
chr11 <- c()
for (each in row){
  if (MainData[each,2]=="11"){
    chr11 <- c(chr11, as.numeric(MainData[each,3]))
  }
}
library(ggplot2)
testdf <- data.frame("initial"= chr11)
testdf$binnedchr11 <- cut(chr11, c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140))
ggplot(testdf)+geom_bar(aes(x=binnedchr11))

freqtable11 <- as.data.frame(table(testdf$binnedchr11))
mean <- mean(freqtable11$Freq)
sd <- sd(freqtable11$Freq)

onesd <- 1+sd
twosd <- 1+2*sd

over68 <- c()
over95 <- c()

##cutting df to only include significant SNPs

row <- c(1:length(freqtable11$Freq))

for (each in row){
  if (freqtable11[each,'Freq']> onesd && freqtable11[each,'Freq'] < twosd){
    over68 <- c(over68, freqtable11[each,'Var1'])
  } else if (freqtable11[each,'Freq'] >= twosd){
    over95 <- c(over95, freqtable11[each,'Var1'] )
  }
}
print(over68)
print(over95)

row <- c(1:length(MainData$SNP))
chr11 <- c()
SNP11 <- c()
for (each in row){
  if (MainData[each,2]=="11"){
    if (MainData[each,3]>50 && MainData[each,3]<=60 || MainData[each,3]>120 && MainData[each,3]<=130){
      chr11 <- c(chr11, as.numeric(MainData[each,3]))
      SNP11 <- c(SNP11, MainData[each, 'SNP'])
    }
  }
}
data.frame("SNP"=SNP11,"Position.bp"=chr11)



##finding SNPs that overlap with potential genes


potSNP11 <- c()
potgenes11 <- c()
row <- c(1:length(Chr11DataNCBI$startpos))
for (each in chr11){
  for (each2 in row){
    if (each >= Chr11DataNCBI[each2,'startpos'] && each <= Chr11DataNCBI[each2,'endpos']){
      potSNP11 <- c(potSNP11, each)
      potgenes11 <- c(potgenes11, Chr11DataNCBI[each2,3])
    }
  }
}
unqpotgenes11<-unique(potgenes11)

Chr11Genes <- data.frame(potSNP11)
Chr11Genes$potgenes11 <- potgenes11



##cleaning data from NCBI Chr 22 Homology 

Chr22DataNCBI <- read_excel("Chr22DataNCBI.xlsx")
Chr22DataNCBI <- Chr22DataNCBI[,1:3]

temp <- c()
generang <- c()
for (each in Chr22DataNCBI$`human position`){ 
  temp <- unlist(strsplit(each, ':'))
  temp2 <- temp[2]
  temp3 <- unlist(strsplit(temp2, " "))
  temp4 <- temp3[1]
  generang <- c(generang, temp4)
}

Chr22DataNCBI$gene.range <- generang

startpos <- c()
endpos <- c()
for (each in Chr22DataNCBI$gene.range){
  temp <- unlist(strsplit(each, "-"))
  temp2 <- gsub(",", "", temp) 
  startpos <- c(startpos, as.numeric(temp2[1])/1000000)
  endpos <- c(endpos, as.numeric(temp2[2])/1000000)
}

Chr22DataNCBI$startpos <- startpos
Chr22DataNCBI$endpos <- endpos



##finding most common SNP range

row <- c(1:length(MainData$SNP))
chr22 <- c()
for (each in row){
  if (MainData[each,2]=="22"){
    chr22 <- c(chr22, as.numeric(MainData[each,3]))
  }
}
library(ggplot2)
testdf <- data.frame("initial"= chr22)
testdf$binnedchr22 <- cut(chr22, c(0, 10, 20, 30, 40, 50))
ggplot(testdf)+geom_bar(aes(x=binnedchr22))

freqtable22 <- as.data.frame(table(testdf$binnedchr22))
mean <- mean(freqtable22$Freq)
sd <- sd(freqtable22$Freq)

onesd <- 1+sd
twosd <- 1+2*sd

over68 <- c()
over95 <- c()

row <- c(1:length(freqtable22$Freq))

for (each in row){
  if (freqtable22[each,'Freq']> onesd && freqtable22[each,'Freq'] < twosd){
    over68 <- c(over68, freqtable22[each,'Var1'])
  } else if (freqtable22[each,'Freq'] >= twosd){
    over95 <- c(over95, freqtable22[each,'Var1'] )
  }
}
print(over68)
print(over95)

##cutting df to only include significant SNPs

row <- c(1:length(MainData$SNP))
chr22 <- c()
SNP22 <- c()
for (each in row){
  if (MainData[each,2]=="22"){
    if (MainData[each,3]>30 && MainData[each,3]<=40){
      chr22 <- c(chr22, as.numeric(MainData[each,3]))
      SNP22 <- c(SNP22, MainData[each, 'SNP'])
    }
  }
}
data.frame("SNP"=SNP22,"Position.bp"=chr22)


##finding SNPs that overlap with potential genes

potSNP22 <- c()
potgenes22 <- c()
row <- c(1:length(Chr22DataNCBI$startpos))
for (each in chr22){
  for (each2 in row){
    if (each >= Chr22DataNCBI[each2,'startpos'] && each <= Chr22DataNCBI[each2,'endpos']){
      potSNP22 <- c(potSNP22, each)
      potgenes22 <- c(potgenes22, Chr22DataNCBI[each2,3])
    }
  }
}


unqpotgenes22<-unique(potgenes22)

Chr22Genes <- data.frame(potSNP22)
Chr22Genes$potgenes22 <- potgenes22


##main analysis
##adding chr data to gene data -- Chr22 has NO GENES and will be removed from now on

Chr11Genes$chr <- 11
Chr6Genes$chr <- "06"


##combining chr 6, 11 dfs

genedata <- data.frame("chr"=c(Chr6Genes$chr, Chr11Genes$chr))
genedata$bp <- c(Chr6Genes$potSNP6, Chr11Genes$potSNP11)
genedata$potgene <- c(Chr6Genes$potgenes6, Chr11Genes$potgenes11)


##adding SNPs to df

row <- c(1:length(MainData$Position.Mb))

temp <- c()
for (each in row){
  for (each2 in genedata$bp){
    if (each2 == MainData$Position.Mb[each]){
      temp <- c(temp, MainData$SNP[each])
    }
  }
}


genedata$SNP <- temp



##adding descriptions to data
##REQUIRES USER INTERACTION
##create excel file titled "NCBIGeneDescrData.xlsx"; create columns "gene" "officialsymbol" "fullname" "description" "pathways" "diseases"; for "gene" column, copy all genes from previous analysis; all columns, import data from NCBI into respective columns; leave pathways column blank


NCBIGeneDescrData <- read_excel("NCBIGeneDescrData.xlsx")

genedata$officialsymbol <- NCBIGeneDescrData$officialsymbol
genedata$fullname <- NCBIGeneDescrData$fullname
genedata$description <- NCBIGeneDescrData$description
genedata$simpdesc <- NCBIGeneDescrData$simple
genedata$diseases <- NCBIGeneDescrData$diseases


##uploading pathways data and adding to genedata
##REQUIRES USER INTERACTION
##create excel file for each gene titled "GENENAMEpath.xlsx" with one column titled "pathways" and import all pathways into column

library(readxl)
BAT2path <- read_excel("BAT2path.xlsx")
HLADRB1path <- HLADRB1path <- read_excel("HLADRB1path.xlsx", sheet = "Sheet2")
LRRC16Apath <- read_excel("LRRC16Apath.xlsx")
SLC17A1path <- read_excel("SLC17A1path.xlsx")
PPP1R10path <- read_excel("PPP1R10path.xlsx")
LTApath <- read_excel("LTApath.xlsx")
SLC44A4path <- read_excel("SLC44A4path.xlsx")
ZKSCAN3path <- read_excel("ZKSCAN3path.xlsx")
ZKSCAN12path <- read_excel("ZKSCAN12path.xlsx")
OR5T2path <- read_excel("OR5T2path.xlsx")
SMTNL1path <- read_excel("SMTNL1path.xlsx")
CCDC15path <- read_excel("CCDC15path.xlsx")
TMEM218path <- read_excel("TMEM218path.xlsx")
genedata$pathways <- c(BAT2path, HLADRB1path, LRRC16Apath, SLC17A1path, PPP1R10path, LTApath, SLC44A4path, ZKSCAN3path, ZKSCAN12path, OR5T2path, SMTNL1path, CCDC15path, TMEM218path)



##cleaning pathways data

temp <- c()
pathways <- c()
for (each in genedata$pathways){
  temp <- unlist(strsplit(each, ","))
  pathways <- c(pathways, temp)
}
pathways <- na.omit(pathways)


##defining new df for pathways

df <- as.data.frame(table(pathways))


##defining new df for diseases
library(writexl)
diseasesdf<- (as.data.frame(table(unlist(strsplit(tolower(genedata$diseases), ",")))))
write_xlsx(diseasesdf, "diseasesdf.xlsx")


##REQUIRES USER INTERACTION
##open diseasesdf.xlsx; define new column "Immune"; if disease is autoimmune, put "1" in row; if not autoimmune, put "0" in row

diseasesdfnew <- read_excel("diseasesdfrevised.xlsx")


##calculating percent of autoimmune diseases in group

immune <- c()
nonimmune <- c()
row <- 1:length(diseasesdfnew$Immune)
for (each in row){
  if (diseasesdfnew$Immune[each]==1){
    immune <- c(immune, diseasesdfnew$Var1[each])
  } else {
    nonimmune <- c(nonimmune, diseasesdfnew$Var1[each])
  }
}

length(immune)/(length(immune)+length(nonimmune))

##importing data about Chr6 and cutting df to only include histocompatibility genes
##finding percent of chr6 SNPs that are in histocompatibility complex (assuming avg gene length of 5000 bps)


chr6table <- read_excel("CHR6TABLE.xlsx")

chr6table$`Begin position in sequence` <- chr6table$`Begin position in sequence`/1000000

row <- c(length(chr6table$`Begin position in sequence`):1)
for (each in row){
  if (!(grepl("Histone", chr6table$`Gene family`[each]))){
    chr6table <- chr6table[-c(each),]
  }
}

chr6table$endpos <- chr6table$`Begin position in sequence`+5

histoSNP <- c()
row <- c(1:length(chr6table$`Begin position in sequence`))
for (each in chr6){
  for (each2 in row){
    if (each>= chr6table$`Begin position in sequence`[each2] && each <= chr6table$endpos[each2]){
      histoSNP <- c(histoSNP, each)
    }
  }
}
histoSNP <- unique(histoSNP)

nonhistoSNP <- c()
for (each in chr6){
  if (!(each %in% histoSNP)){
    nonhistoSNP <- c(nonhistoSNP, each)
  }
}


histoSNP <- sort(histoSNP)
nonhistoSNP <- sort(nonhistoSNP)

length(histoSNP)/(length(nonhistoSNP)+length(histoSNP))



##adding data about which pathways are in each gene

vect <- c()
for (each in df$pathways){
  if (each %in% BAT2path$pathways){
    vect <- c(vect, ", BAT2")
  } else
    vect <- c(vect, "no")
}
vect2 <- c()
for (each in df$pathways){
  if (each %in% HLADRB1path$pathways){
    vect2 <- c(vect2, "HLADRB1")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(vect, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% LRRC16Apath$pathways){
    vect2 <- c(vect2, "LRRC16A")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% SLC17A1path$pathways){
    vect2 <- c(vect2, "SLC17A1")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% PPP1R10path$pathways){
    vect2 <- c(vect2, "PPP1R10")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% LTApath$pathways){
    vect2 <- c(vect2, "LTA")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% SLC44A4path$pathways){
    vect2 <- c(vect2, "SLC44A4")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% ZKSCAN3path$pathways){
    vect2 <- c(vect2, "ZKSCAN3")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% ZKSCAN12path$pathways){
    vect2 <- c(vect2, "ZKSCAN12")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% OR5T2path$pathways){
    vect2 <- c(vect2, "OR5T2")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% SMTNL1path$pathways){
    vect2 <- c(vect2, "SMTNL1")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% CCDC15path$pathways){
    vect2 <- c(vect2, "CCDC15")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

vect2 <- c()
for (each in df$pathways){
  if (each %in% TMEM218path$pathways){
    vect2 <- c(vect2, "TMEM218")
  } else
    vect2 <- c(vect2, "no")
}
path <- paste(path, vect2, sep=", ")

temp <- c()

temp <- gsub("no", "", path)

temp2 <- gsub(", ,",",",temp)
temp3 <- gsub(", ,",",",temp2)
temp4 <- gsub(", ,",",",temp3)
temp5 <- gsub(", ,",",",temp4)

temp6 <- sub(".{2}", "", temp5)
temp7 <- sub(".{2}$", "", temp6)

df$genes <- temp7


##adding percent column

df$percent <- df$Freq/length(genedata$potgene)*100


##adding plot for amounts of pathways that are present in certain percents of genes (pathways in only 1 gene have been ommitted)

percents <- c()

for (each in df$Freq){
  if (each >1){
    percents <- c(percents, each)
  }
}
percents <- percents/length(genedata$potgene)*100
percents <- as.data.frame(percents)

ggplot(percents)+ geom_bar(aes(x=percents))


##finding common words in pathway titles

rem <- c()
commonwords <- c()
temp <- c()
temp <- unlist(strsplit(pathways, " "))
temp <- gsub("\\(", "", temp)
temp <- gsub("\\)", "", temp)
temp <- gsub("\\[", "", temp)
temp <- gsub("\\]", "", temp)
temp <- gsub("/", "", temp)
temp <- gsub("\\\\", "", temp)
temp <- gsub("\\-", "", temp)
temp <- na.omit(temp)

for (each in temp){
  temp2 <- unlist(strsplit(each, ""))
  if (length(temp2)<4 && each != "RNA" && each != "HPA" && each != "MHC" && each != "CD4"){
    rem <- c(rem, each)
  } else {
    commonwords <- c(commonwords, each)
  }
}
finaldata <- as.data.frame(table(tolower((commonwords))))

finaldata <- finaldata[order(-finaldata$Freq),]


##saving an excel sheet with word list and frequencies

library(writexl)
finaldata$relfreq <- finaldata$Freq/max(finaldata$Freq)
write_xlsx(finaldata, "finaldata.xlsx")


##finding significant words and redefining data to only include these

mean <- mean(finaldata$Freq)
sd <- sd(finaldata$Freq)

onesd <- mean+sd
twosd <- mean+2*sd

over68 <- c()
over95 <- c()

row <- c(1:length(finaldata$Var1))

for (each in row){
  if (finaldata[each,'Freq']> onesd && finaldata[each,'Freq'] < twosd){
    over68 <- c(over68, finaldata[each,'Var1'])
  } else if (finaldata[each,'Freq'] >= twosd){
    over95 <- c(over95, finaldata[each,'Var1'] )
  }
}
relevant <- length(over68)+length(over95)


finaldata <- finaldata[1:relevant,]

write_xlsx(finaldata, "finaldata.xlsx")

##REQUIRES USER INTERACTION
##open finaldata.xlsx; name new column "kept"; insert "1" for words to continue in analysis and "0" for words to omit from analysis; save file as finaldatarevised.xlsx

finaldatanew <- read_excel("finaldatarevised.xlsx")


##removing "0" words

row <- length(finaldatanew$kept):1

for (each in row){
  if (finaldatanew$kept[each] == 0){
    finaldatanew <- finaldatanew[-c(each),]
  }
}

finaldata$numforgraph <- 1:length(finaldata$Var1)


temp <- c()
row <- 1:length(finaldata$Var1)
for (each in finaldatanew$Var1){
  for (each2 in row){
    if (each == finaldata$Var1[each2]){
      temp <- c(temp, finaldata$numforgraph[each2])
    }
  }
}
finaldatanew$numforgraph <- temp



##plots for (1) counts of all significant words and (2) counts of all chosen words


library(ggplot2)

ggplot(finaldata, color= station, fill='black', aes(x=Var1, y=Freq))+
  geom_col(position='dodge')+
  ylab("Frequency")+
  xlab("Word")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))

ggplot(finaldatanew, color= station, fill='black', aes(x=Var1, y=Freq))+
  geom_col(position='dodge')+
  ylab("Frequency")+
  xlab("Word")









