rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if(outcome == "heart attack") column <- 11
  else if(outcome == "heart failure") column <- 17
  else if(outcome == "pneumonia") column <- 23
  else stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data[,column] <- as.numeric(data[,column])
  states <- names(table(data$State))
  
  results <- NULL
  for(state in states) {
    search.subset <- subset(data, data$State == state)
    search.subset[,column] <- as.numeric(search.subset[,column])
    bad.entries <- is.na(search.subset[,column])
    complete.subset <- subset(search.subset, !bad.entries)
    
    if(num == "best") dex <- 1
    if(num == "worst") dex <- length(complete.subset[,column])
    
    ordered.rates <- sort(complete.subset[,column])
    find.name <- complete.subset[,column] == ordered.rates[dex]
    hospital <- complete.subset$Hospital.Name[find.name]
    
    ## generate final list
    results <- append(results, sort(hospital)[1])
  }
  
  ## return final
  final <- data.frame(cbind(hospital = results, state = states))

}