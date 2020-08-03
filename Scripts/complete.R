# Reads a directory full of files and reports the number of completely observed 
# cases in each data file. 
#
# The function returns a data frame where the first column is the name of the 
# file and the second column is the number of complete cases.
complete <- function(directory = "data/specdata", id = 1:332){
  ids <- list()
  nobs <- list()
  count <- 1
  for(i in id){
    data <- read.csv(sprintf("%s/%03d.csv", directory, i))
    temp_nobs <- sum(complete.cases(data))
    nobs[count] <- temp_nobs
    ids[count] <- i
    count <- count + 1
  }
  df <- data.frame(cbind(ids, nobs))
  colnames(df)[colnames(df) == "ids"] <- "id"
  df
}