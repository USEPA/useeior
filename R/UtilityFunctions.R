
#' Start logging 
startLogging <- function (){
  #http://logging.r-forge.r-project.org/sample_session.php
  logging::basicConfig()
  # Define output folder
  #dir.create(paste("Model Builds/", modelname, sep = ""), recursive = TRUE) # meant to be flexible up to users
  #outputfolder <- paste("Model Builds/", modelname, sep = "")
  #Write logs to file in model build folder
  #logtimestamp = Sys.Date()
  #logfilename = paste(outputfolder,"/",logtimestamp,".log",sep="")
  #addHandler(writeToFile, file=logfilename, level='INFO')
}

#' Join strings with slashes
joinStringswithSlashes <- function(...) {
  items <- list(...)
  str <- sapply(items, paste, collapse = '/')
  str <- tolower(str)
  return(str)
}
