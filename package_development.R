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

  # add dependencies to the description and load them here 

    pkgdpns <- c("RODBC", "pedigree", "plyr", "MASS", "VGAM")

    for(i in 1:length(pkgdpns)){
      devtools::use_package(pkgdpns[i], "Imports", loc)
    }


  devtools::load_all(devtools::as.package(loc))
  devtools::document(devtools::as.package(loc))


