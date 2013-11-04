rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid.states <- names(table(data$State))
  if(!(state %in% valid.states)) stop("invalid state")
  
  if(outcome == "heart attack") column <- 11
  else if(outcome == "heart failure") column <- 17
  else if(outcome == "pneumonia") column <- 23
  else stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank 30-day death rate
  search.subset <- subset(data, data$State == state)
  search.subset[,column] <- as.numeric(search.subset[,column])
  bad.entries <- is.na(search.subset[,column])
  complete.subset <- subset(search.subset, !bad.entries)
  
  if(num == "best") num <- 1
  if(num == "worst") num <- length(complete.subset[,column])
  
  ordered.rates <- sort(complete.subset[,column])
  find.name <- complete.subset[,column] == ordered.rates[num]
  hospital <- complete.subset$Hospital.Name[find.name]
  
  ## return final
  sort(hospital)[1]
}