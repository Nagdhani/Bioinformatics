#user input for file path (only use one backslash)
filename <- readline(prompt="Enter file path: ")
#upload user input as dataframe
filename <- read.csv(filename,header=TRUE)
#create vector named LEEs for Paper ID column. 
LEEs <- filename$Paper.ID
#substitute "[" "]" "'" and space symbols with "nothing" in the data frame.
filename$Paper.ID <- gsub("\\]", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\[", "", filename$Paper.ID)
filename$Paper.ID <- gsub("\\'", "", filename$Paper.ID)
filename$Paper.ID <- gsub(" ", "", filename$Paper.ID)
#c(length(LEEs)) used to correspond to a row in the dataframe in each iteration of the for loop
#vector of strings is split into individual characters with strsplit. Unlist used to revectorize the list. 
#number of unique paper IDs was found for each row and row was deleted if the unique values = 1
for (each in c(length(LEEs):1)){
  if (length(unique(unlist(strsplit(filename$Paper.ID[each], ",")))) == 1){
    filename <- filename[-c(each),]
  }
}


