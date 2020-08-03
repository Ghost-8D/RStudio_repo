# Calculates the mean of a pollutant (sulfate or nitrate) across a specified 
# list of monitors. 
# 
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
# particulate matter data from the directory specified in the 'directory' 
# argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA.
pollutantmean <- function(directory = "data/specdata", pollutant, id = 1:332){
  sum <- 0.0
  count <- 0.0
  if (pollutant == "sulfate" || pollutant == "nitrate"){
    for(i in id){
      data <- read.csv(sprintf("%s/%03d.csv", directory, i))
      use <- !is.na(data[pollutant])
      clean_data <- data[use, pollutant]
      sum <- sum + sum(clean_data)
      count <- count + length(clean_data)
    }
    sum/count
  } else{
    print("Wrong pollutant name given! Available options: sulfate, nitrate")
  }
}

