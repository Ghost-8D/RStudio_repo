## The function reads the outcome-of-care-measures.csv file and returns a character 
## vector (the name of the hospital) that has the ranking specified by the num 
## argument, based on the mortality value for the specified outcome in that state. 
## 
## The function takes two arguments: 
## - state : the 2-character abbreviated name of a state
## - outcome : outcome name (either "heart attack", "heart failure" or "pneumonia") 
## - num : the ranking of a hospital (either "best", "worst", non-zero integer)
## 
## Note: The function throws an error if either state or outcome is not valid.
## 
## Handling ties: If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order and the first 
## hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” 
## are tied for best, then hospital “b” should be returned).
##
## Usage examples:
## ==============
## Example with best:
## > rankhospital("TX", "heart failure", "best")
## [1] "FORT DUNCAN MEDICAL CENTER"
## 
## Example with 1st index:
## > rankhospital("TX", "heart failure", 1) 
## [1] "FORT DUNCAN MEDICAL CENTER"
## 
## Example with worst
## > rankhospital("TX", "heart failure", "worst")
## [1] "ETMC CARTHAGE"
## 
## Example with negative index 
## > rankhospital("TX", "heart failure", -1)
## [1] "ETMC CARTHAGE"
## 
## Example with incorrect argument
## > rankhospital("TX", "hart failure", 1)
## Error in rankhospital("TX", "hart failure", 1) : invalid outcome
## 
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    directory <- file.path("data", "rprog_data_ProgAssignment3-data")
    input_data <- read.csv(file.path(directory, "outcome-of-care-measures.csv"), 
                                     colClasses = "character")
    
    ## Check that state and outcome are valid
    avail_states <- unique(input_data$State)
    if (! state %in% avail_states){
        stop("invalid state")
    }
    if (! outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    
    ## Based on the selected outcome find the best hospital
    name_col <- 2
    state_col <- 7
    
    if (outcome == "heart attack"){
        outcome_col <- 11
    } else if (outcome == "heart failure"){
        outcome_col <- 17
    } else if (outcome == "pneumonia"){
        outcome_col <- 23
    }
    
    # Convert to numeric and suppress warning for NAs
    suppressWarnings(input_data[, outcome_col] <- as.numeric(input_data[, outcome_col]))
    
    # Get only relevant columns
    target_data <- input_data[, c(name_col, state_col, outcome_col)]
    
    # Exclude rows with NAs
    use <- complete.cases(target_data)
    
    # Get columns for specified state
    use_state <- target_data[, 2] == state
    rel_data <- target_data[use & use_state, ]
    
    # Rename columns for easier handling
    names(rel_data) <- c("Hospital", "State", "Mortality")
    
    # Order by ascending mortality and then by ascending hospital names
    # For descending order add a '-' in front of the column data
    index <- order(rel_data$Mortality, rel_data$Hospital)
    sorted_data <- rel_data[index, ]
    
    ## Return hospital name in that state with the given rank 30-day death rate
    if (num == "best"){
        return(sorted_data[1, 1])
    }
    if (num == "worst"){
        return(sorted_data[nrow(sorted_data), 1])
    }
    
    ## If num is negative start counting from the bottom
    if (num < 0){
        num <- nrow(sorted_data) + 1 + num
    }
    
    ## If the number is bigger than the number of rows or is zero return NA
    if (abs(num) > nrow(sorted_data) || num == 0){
        return(NA)
    }
    sorted_data[num, 1]
}