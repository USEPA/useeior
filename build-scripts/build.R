#Rtools must be installed on the machine which is downloaded and installed a binary on Windows

library(devtools)

#Use Ctrl+Shift+E to run R CMD package check in RStudio (Windows)
#this is the same as devtools::check()


#First time
#devtools::create("useeior")


#Create namespaces with roxygen
library(roxygen2)
#Create Rd documentation files in /man for docs
devtools::document()

#Use this to test loading data that you added to the package
devtools::load_all()

#to build it
#devtools::build()


#Use "Install and Restart" to install the package locally

