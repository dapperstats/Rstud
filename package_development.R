################################################################################
#
# This script facilitates development of the Rstud package
#
#
################################################################################

  # preliminaries

    library(devtools)
    loc <- getwd()

  #
  #  NOTE: This code only needs to be run once!
  #         Currently commented out
  #

  #  # create the barebones package structure 
  
  #    devtools::create(loc)  

    for(i in 1:length(pkgdpns)){
      devtools::use_package("survival", "Imports", loc)
    }