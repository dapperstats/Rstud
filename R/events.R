#' Determine relevant events for everyone
#' 
#' @param studbook Studbook
#' @param exclude Character vector of StudbookIDs to exclude from the events
#' @param starting_date Character of the starting date (YYYY-MM-DD)
#' @param ending_date Character of the ending date (YYYY-MM-DD)
#' @param starting_buffer Amount of time to not include from the starting date
#' @param ending_buffer Amount of time not to include from the ending date
#' @param buffer_units Currently only supported for "days"
#' @return list of table of (non-transfer) events and table of transfer events
#' 
#' @export 
#'
events <- function(studbook = NULL, exclude = NULL, starting_date = NULL, 
                   ending_date = NULL, starting_buffer = 0, ending_buffer = 0,
                   buffer_units = "days"){

  ind <- as.character(studbook$Master$StudbookID)
  if(length(exclude) > 0){
    ind <- ind[-which(ind %in% exclude)]
  }
  nind <- length(ind) 
  dwindow <- create_date_window(starting_date, ending_date, starting_buffer,
                                ending_buffer, buffer_units)

  wcday <- as.Date(rep(NA, nind))
  wclocation <- rep(NA, nind)
  birthday <- as.Date(rep(NA, nind))
  birthlocation <- rep(NA, nind)
  deathday <- as.Date(rep(NA, nind))
  deathlocation <- rep(NA, nind)
  ltfday <- as.Date(rep(NA, nind))
  ltflocation <- rep(NA, nind)

  tranind <- NULL
  tranday <- as.character(NULL)
  tranyear <- as.character(NULL)
  trandestlocation <- NULL
  tranoriglocation <- NULL

  for(i in 1:nind){
  
    id_in <- studbook$Event$StudbookID == ind[i]
    events_in <- check_dates(as.Date(studbook$Event$TranDate), dwindow)
    events_ind <- studbook$Event[which(id_in & events_in), ]

    WCs <- which(events_ind$TranCode == "Wild Capture")   
    if(length(WCs) > 0){
      wcday[i] <- as.Date(events_ind$TranDate[WCs])
      wclocation[i] <- as.character(events_ind$Location[WCs])
    }

    Bs <- which(events_ind$TranCode %in% c("Birth", "Hatch"))
    if(length(Bs) > 0){
      birthday[i] <- as.Date(events_ind$TranDate[Bs])
      birthlocation[i] <- as.character(events_ind$Location[Bs])
    } 

    Ds <- which(events_ind$TranCode == "Death")
    if(length(Ds) > 0){
      deathday[i] <- as.Date(events_ind$TranDate[Ds])
      deathlocation[i] <- as.character(events_ind$Location[Ds])
    } 

    gLTFs <- which(events_ind$TranCode == "Go LTF")
    if(length(gLTFs) > 0){
      ltfday[i] <- as.Date(events_ind$TranDate[gLTFs])
      ltflocation[i] <- as.character(events_ind$Location[gLTFs])
    } 
    
    Ts <- which(events_ind$TranCode == "Transfer")
    if(length(Ts) > 0){
      tranind <- c(tranind, rep(ind[i], length(Ts)))
      Tdates <- as.character(events_ind$TranDate[Ts])
      tranday <- c(tranday, Tdates)
      for(j in 1:length(Ts)){
        tranyear <- c(tranyear, strsplit(Tdates, "-")[[j]][1])
      }
      dest <- as.character(events_ind$Location[Ts])
      trandestlocation <- c(trandestlocation, dest)
      orig <- rep(NA, length(Ts))
      for(j in 1:length(orig)){
        orig[j] <- as.character(events_ind$Location[Ts[j] - 1])
      }
      tranoriglocation <- c(tranoriglocation, orig)

    }
  }    

  eventbyindtable <- data.frame(ind, wcday, wclocation, birthday, 
                                birthlocation, deathday, deathlocation, 
                                ltfday, ltflocation)
  transfertable <- data.frame(tranind, tranday, tranoriglocation, 
                              trandestlocation, tranyear)
  list(events = eventbyindtable, transfers = transfertable)
}