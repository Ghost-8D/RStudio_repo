# Takes a directory of data files and a threshold for complete cases and 
# calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater 
# than the threshold.
#
# Returns a vector of correlations for the monitors that meet the threshold 
# requirement. If no monitors meet the threshold requirement, then the function 
# will return a numeric vector of length 0.
corr <- function(directory = "data/specdata", threshold = 0){
  res <- numeric()
  count <- 1
  for(file in dir(directory)){
    data <- read.csv(file.path(directory, file))
    use <- complete.cases(data)
    temp_cases <- sum(use)
    if (temp_cases > threshold){
      res[count] <- cor(data[use, "sulfate"], data[use, "nitrate"])
      count <- count + 1
    }
  }
  res
}