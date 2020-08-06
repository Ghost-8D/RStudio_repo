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
    if (num > nrow(sorted_data)){
        return(NA)
    }
    sorted_data[num, 1]
}