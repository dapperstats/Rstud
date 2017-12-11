#' Read in and Edit Institution List
#' 
#' @param institution_file Character(s) of the name(s) of the .FED file(s) 
#'  that are to be used for the insitutional definition
#' @param add_institutions Characters(s) of the name(s) of the instituions 
#'  that are to be added to the included insitutions
#' @param drop_institutions Characters(s) of the name(s) of the instituions
#'  that are to be dropped to the included insitutions
#' @return Character vector of institutions defining the population
#' @examples
#' 
#' 


create_institution_window <- function(institution_file = NULL, 
                                    add_institutions = NULL, 
                                    drop_institutions = NULL){

  if(length(institution_file) == 0)
    return(print("No FED file given."))    

  insts <- NULL
  for(i in 1:length(institution_file)){
    insts <- c(insts, 
               scan(file = paste(gsub("[\\]", "/", Sys.getenv("HOME")), 
                                 "/PopLink/Federation Files/", 
                                 institution_file[i], ".FED" , sep = ""), 
                    what = "character", sep = "\n", quiet = TRUE))
  }
  

  if(length(add_institutions) > 0)
    insts <- c(insts, add_institutions)

  if(length(drop_institutions) > 0)
    insts <- insts[-which(insts %in% drop_institutions)]

  insts <- sort(unique(insts))

  return(insts)

}

#' Apply Institutional Window 
#' 
#' @param locations Character vector of locations to be tested
#' @param location_window Character vector defining the locations of interest
#' @return Logical vector of whether each location is input vector is one 
#'  of interest
#' @examples
#' 

check_institutions <- function(locations = NULL, location_window = NULL){

  if(length(location_window) == 0)
    return(NA)    

  in_out <- locations %in% location_window
  
  return(in_out)
}
