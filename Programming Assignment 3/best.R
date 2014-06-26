best <- function(state , outcome){
    ## checking if the outcomes is a valid one.
    outcomes = c("heart attack", "heart failure", "pneumonia")
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(state %in% data$State == FALSE) stop("invalid state")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
    
    data <- data[,c(2,7,11,17,23)]
    names(data) <- list("name" , "state" , "heart attack", "heart failure", "pneumonia")
    
    #just getting the releavant data set for that state
    data <- data[data$state == state , ]
    
    #Coercing the numbers into integers.
    data[ , outcome] <- as.numeric(data[ , outcome ] )
    
    #Sorting my data frame according to the names.
    data <- data[order(data[,1]) , ]
    
    ## Getting the list of the relevant data from the file.
    vals <- data[, outcome]
    rowNum <- which.min(vals) ## getting index of the minima.
    
    ## Return hospital name in that state with lowest 30-day death rate.
    data[rowNum,"name" ]
}