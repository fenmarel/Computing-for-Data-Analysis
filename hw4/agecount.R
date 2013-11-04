agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) stop("invalid age")
  
  ## Read "homicides.txt" data file
  data <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is given
  ## Return integer containing count of homicides for that age
  length(grep(paste0("<dd>(.*) ", age, " years old</dd>"), data, ignore.case = TRUE))
}