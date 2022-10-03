#user input for file path (only use one backslash)
filename <- readline(prompt="Enter file path: ")
#upload user input as dataframe
filename <- read.csv(filename,header=TRUE)
#create vector for Paper ID column 
LEEs <- filename$Paper.ID
#substitute "[" "]" "'" and space symbols with "nothing"
filename$Paper.ID <- gsub("\\]", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\[", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\'", "", filename$Paper.ID)
filename$Paper.ID <- gsub(" ", "", filename$Paper.ID)
#vector of strings is split into individual characters with strsplit. Unlist used to revectorize the list. 
#number of unique paper ID was found for each row and row was deleted if the unique values = 1
for (each in c(length(LEEs):1)){
  if (length(unique(unlist(strsplit(filename$Paper.ID[each], ",")))) == 1){
    filename <- filename[-c(each),]
  }
}


