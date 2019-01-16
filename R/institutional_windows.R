#' Read in and Edit Institution List
#' 
#' @param institution_file Character(s) of the name(s) of the .FED file(s) 
#'  that are to be used for the insitutional definition
#' @param add_institutions Characters(s) of the name(s) of the instituions 
#'  that are to be added to the included insitutions
#' @param drop_institutions Characters(s) of the name(s) of the instituions
#'  that are to be dropped to the included insitutions
#' @return Character vector of institutions defining the population
#' 
#' @export
#' 
create_institution_window <- function(institution_file = NULL, 
                                    add_institutions = NULL, 
                                    drop_institutions = NULL){

  if(length(institution_file) == 0){
    stop("No FED file given.")
  }

  insts <- NULL
  for(i in 1:length(institution_file)){

    fl1 <- gsub("[\\]", "/", Sys.getenv("HOME"))
    fl2 <- "/PopLink/Federation Files/"
    fn <- paste0(institution_file[i], ".FED")
    fp <- paste0(fl1, fl2, fn)
    insts_i <- scan(file = fp, what = "character", sep = "\n", quiet = TRUE)
    insts <- c(insts, insts_i)
  }
  
  insts <- c(insts, add_institutions)
  insts <- insts[!(insts %in% drop_institutions)]

  sort(unique(insts))
}

#' Apply Institutional Window 
#' 
#' @param locations Character vector of locations to be tested
#' @param location_window Character vector defining the locations of interest
#' @return Logical vector of whether each location is input vector is one 
#'  of interest
#' 
#' @export 
#'
check_institutions <- function(locations = NULL, location_window = NULL){
  if(length(location_window) == 0){
    return(NA)
  }
  locations %in% location_window
}
