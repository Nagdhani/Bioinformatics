file <- readline(prompt="Enter file path: ")
 #uploadbfile as dataframe
filename <- read.csv(file,header=TRUE)
LEEs <- filename$Paper.ID
filename$Paper.ID <- gsub("\\]", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\[", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\'", "", filename$Paper.ID)
filename$Paper.ID <- gsub(" ", "", filename$Paper.ID)
for (each in c(length(LEEs):1)){
  if (length(unique(unlist(strsplit(filename$Paper.ID[each], ",")))) == 1){
    filename <- filename[-c(each),]
  }
}

