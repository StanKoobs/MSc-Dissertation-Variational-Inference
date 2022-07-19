##### In this script we define the packages to be used ####################

# List with the packages which we need
ListofPackages <-c("tidyverse", "mvtnorm", "LaplacesDemon", "cowplot")
NewPackages <- ListofPackages[!(ListofPackages %in% 
                                  installed.packages()[,"Package"])]

if (length(NewPackages) > 0) {
  install.packages(NewPackages)
}

lapply(ListofPackages, require, character.only = TRUE)

# No need to keep these variables
rm(ListofPackages, NewPackages)
