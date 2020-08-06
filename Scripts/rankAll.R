## This function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the 
## ranking specified in num. For example the function call rankall("heart attack", 
## "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. 
## The function should return a value for every state (some may be NA). The first 
## column in the data frame is named hospital, which contains the hospital name, 
## and the second column is named state, which contains the 2-character abbreviation 
## for the state name. Hospitals that do not have data on a particular outcome 
## should be excluded from the set of hospitals when deciding the rankings.
## 
## The function takes two arguments: 
## - outcome : outcome name (either "heart attack", "heart failure" or "pneumonia") 
## - num : the ranking of a hospital (either "best", "worst", or positive integer)
## 
## Note: The function throws an error if either num or outcome is not valid.
## 
## Handling ties: If there is a tie for the best hospital for a given outcome, 
## then the hospital names should be sorted in alphabetical order and the first 
## hospital in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” 
## are tied for best, then hospital “b” should be returned).
##
## Usage examples:
## ==============
## Example with best (default value for num):
## > tail(rankall("heart failure"), 5)
## Hospital State
## 50           SPRINGFIELD HOSPITAL    VT
## 51      HARBORVIEW MEDICAL CENTER    WA
## 52 AURORA ST LUKES MEDICAL CENTER    WI
## 53      FAIRMONT GENERAL HOSPITAL    WV
## 54     CHEYENNE VA MEDICAL CENTER    WY
## 
## Example with integer num
## > head(rankall("heart attack", 20), 6)
## Hospital State
## 1                                 <NA>    AK
## 2       D W MCMILLAN MEMORIAL HOSPITAL    AL
## 3    ARKANSAS METHODIST MEDICAL CENTER    AR
## 4  JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
## 5                SHERMAN OAKS HOSPITAL    CA
## 6             SKY RIDGE MEDICAL CENTER    CO
## 
## Example with worst
## > tail(rankall("pneumonia", "worst"), 3)
## Hospital State
## 52 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
## 53                     PLATEAU MEDICAL CENTER    WV
## 54           NORTH BIG HORN HOSPITAL DISTRICT    WY
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    directory <- file.path("data", "rprog_data_ProgAssignment3-data")
    input_data <- read.csv(file.path(directory, "outcome-of-care-measures.csv"), 
                           colClasses = "character")
    
    ## Check that outcome is valid
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
    rel_data <- target_data[use, ]
    
    # Rename columns for easier handling
    names(rel_data) <- c("Hospital", "State", "Mortality")
    
    # Order by ascending mortality and then by ascending hospital names
    # For descending order add a '-' in front of the column data
    sorted_data <- rel_data[order(rel_data$State, rel_data$Mortality, rel_data$Hospital), ]
    
    ## Return hospital name in that state with the given rank 30-day death rate
    if (num == "best"){
        return(aggregate(. ~ State, sorted_data, FUN=function(d){ return(d[1]) })[, c(2, 1)])
    }
    if (num == "worst"){
        return(aggregate(. ~ State, sorted_data, FUN=function(d){ return(d[length(d)]) })[, c(2, 1)])
    }
    
    ## The num argument must be positive 
    if (num <= 0){
        stop("invalid num")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the (abbreviated) state name
    aggregate(. ~ State, sorted_data, FUN=function(d){ return(d[num]) })[, c(2, 1)]
}