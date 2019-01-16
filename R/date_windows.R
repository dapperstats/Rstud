#' Create Date Window
#' 
#' @param starting_date Character of the starting date (YYYY-MM-DD)
#' @param ending_date Character of the ending date (YYYY-MM-DD)
#' @param starting_buffer Amount of time to not include from the starting date
#' @param ending_buffer Amount of time not to include from the ending date
#' @param buffer_units Currently only supported for "days"
#' @return Beginning and ending days of the date window.
#' 
#' @export 
#'
create_date_window <- function(starting_date = NULL, ending_date = NULL,
                               starting_buffer = 0, ending_buffer = 0,
                               buffer_units = "days"){

  if(length(ending_date) == 0){
    today <- Sys.Date()
    msg <- paste0("No ending date input, today (", today ,") is being used.")
    message(msg)
    ending_date <- today
  }
  ending_date <- as.Date(ending_date)
  if(length(starting_date)==0){
    message("No starting date input, 1980-01-01 is being used.")
    starting_date <- "1980-01-01"
  }
  starting_date <-as.Date(starting_date)

  if(buffer_units != "days"){
    stop("No support for non-day units for the buffer yet")
  }

  c(starting_date + starting_buffer, ending_date - ending_buffer)
}

#' Apply Date Window 
#' 
#' @param dates Character vector of locations to be tested
#' @param date_window Character vector defining the dates of interest
#' @return Logical vector of whether each date in input vector is one 
#'  of interest
#' 
#' @export 
#'
check_dates <- function(dates = NULL, date_window = NULL){
  if(length(date_window) == 0){
    return(NA)
  }
  dates %in% seq(date_window[1], date_window[2], 1)
}
