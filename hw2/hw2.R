getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
        
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
        
  ## set wd
  d <- strsplit(getwd(), "/")
  if((d[[1]][length(d[[1]])]) != directory) {
    setwd(directory)
  }  
  
  ## generate filename xxx.csv
  if(nchar(id) == 1) {
    file <- paste0("00", toString(id), ".csv")
  } else if(nchar(id) == 2) {
      file <- paste0("0", toString(id), ".csv")
  } else {
      file <- paste0(toString(id), ".csv")
  }
        
  ## open data
  data <- read.csv(file)
  
  ## summary
  if(summarize == TRUE) {
    print summary(data)
  }
  
  ## return
  data
  
}