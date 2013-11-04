best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	valid.states <- names(table(data$State))
	if(!(state %in% valid.states)) stop("invalid state")

	if(outcome == "heart attack") column <- 11
	else if(outcome == "heart failure") column <- 17
	else if(outcome == "pneumonia") column <- 23
	else stop("invalid outcome")

	## search hospital name in that state with lowest 30-day death rate
	search.subset <- subset(data, data$State == state)
	search.subset[,column] <- as.numeric(search.subset[,column])
	bad.entries <- is.na(search.subset[,column])
	complete.subset <- subset(search.subset, !bad.entries)
	minimum.rate <- min(complete.subset[,column])
	find.name <- complete.subset[,column] == minimum.rate
	hospital <- complete.subset$Hospital.Name[find.name]
  
	## return final
	sort(hospital)[1]
	
}