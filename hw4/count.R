count <- function(cause = NA) {
## Check that "cause" is non-NA; else throw error
## Check that specific "cause" is allowed; else throw error
	causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
	if (!(cause %in% causes)) stop("invalid cause")

## Read "homicides.txt" data file
	data <- readLines("homicides.txt")

## Extract causes of death
## Return integer containing count of homicides for that cause
	length(grep(paste0("<dd>Cause: ", cause, "</dd>"), data, ignore.case = TRUE))
}