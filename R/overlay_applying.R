#' Apply Selected Overlay
#' 
#' @param studbook Studbook name
#' @param overlay_to_use Character name of which overlay to use
#' @param verbose Logical of whether or not to print details
#' @param remove Logical of whether or not to remove the (applied) overlay
#'  components
#' @param add_IsHypothetical Logical of whether or not to add "IsHypothetical"
#'  with a value of 0 in instances where the overlay is not applied
#' @return Studbook as a named listed of database tables with overlay applied
#' @examples
#' 
#' 

apply_overlay <- function(studbook = NULL, overlay_to_use = NULL, 
                          verbose = TRUE, remove = TRUE, 
                          add_IsHypothetical = TRUE){

  OLcomponents <- grep("Overlay", names(studbook))
  REGcomponents <- (1:length(names(studbook)))[-OLcomponents]

  output <- studbook
  OLnames <- NULL


  # check the requested overlay_to_use against the overlays that exist

    OverlayDetails <- overlayExamine(studbook = studbook, full_return = FALSE)

  # trying to come up with a stop-gap to deal with "none"
  #  this will need to be improved on in the future, for sure

    if(class(OverlayDetails) != "character"){
      OLnames <- as.character(OverlayDetails$OverlaySummary$Name)
    }

    if(length(overlay_to_use) == 0 & length(OLnames) > 0){
        cat(cat("No overlay selected, yet overlay(s) exist(s): \n \t"), 
            cat(OLnames, sep="\n \t"), 
            cat(paste("Which overlay would you like to apply? "),  
              "If no overlay is desired, enter the word None. \n", sep = ""))
        overlay_to_use <- readline(" ")      
    }
  
    if(length(OLnames) == 0 & length(overlay_to_use) == 0)    
      overlay_to_use <- "None"

    OL <- which(OLnames == overlay_to_use)


  # if the user requested an overlay that doesn't exist, say so and end 
  #  the function 
  #  [in future, make this be so there can be a talk back and give them an 
  #   option, rather than just error/end]

    if(length(OL) == 0 & overlay_to_use != "None")
      return(print("Requested overlay does not exist."))

  # if the user wants to not apply any overlays, add IsHypothetical 
  #  (with values = 0) to all tables (if desired)

    if(overlay_to_use == "None"){
      if(verbose == TRUE)
        print("No overlay applied.")

      if(add_IsHypothetical == TRUE){
        for(i in 1:length(REGcomponents)){
          tt <- which(names(studbook) == names(studbook)[REGcomponents[i]])
          output[[i]] <- data.frame(studbook[[tt]], 
                              IsHypothetical = rep(0, nrow(studbook[[tt]])))
      }
    }
  }


  # now run the sub functions, depending on which tables are part of the 
  #  database only written in the ones I want/need now.  should probably code 
  #  up others at some point

    if(OverlayToUse != "None"){

      overlay_UID_to_apply <- (Studbook$OverlayInformation)$GeneratedGUID[OL]

      DBcomponents <- names(Studbook)[-OLcomponents]

      if(length(which(DBcomponents %in% "Master"))>0)
        output$Master <- apply_overlay_master(studbook, 
                                    overlay_UID_to_apply = overlayUID_toApply, 
                                    add_IsHypothetical = add_IsHypothetical)
	
      if(length(which(DBcomponents %in% "Event"))>0)
        output$Event <- apply_overlay_event(Studbook, 
                                    overlay_UID_to_apply = overlayUID_toApply, 
                                    add_IsHypothetical = add_IsHypothetical)

      if(length(which(DBcomponents %in% "Sex"))>0)
        output$Sex <- apply_overlay_sex(Studbook, 
                                    overlay_UID_to_apply = overlayUID_toApply, 
                                    add_IsHypothetical = add_IsHypothetical)

      if(verbose == TRUE)
        print(paste("Overlay [", OverlayToUse, "] applied.", sep = ""))

    }

  # if wanted, remove the overlay components from the output

    if(remove == TRUE){

      output2 <- output
      output <- vector("list", length(REGcomponents))
	
      for(i in 1:length(REGcomponents))
        output[[i]] <- output2[[REGcomponents[i]]]

      names(output) <- names(output2)[REGcomponents]	
    }
	
  return(output)

}
	

#' Apply the master table overlay
#' 
#' @param studbook Studbook name
#' @param overlay_UID_to_apply ID of the overlay to apply 
#' @param add_IsHypothetical Logical of whether or not to add "IsHypothetical"
#'  with a value of 0 in instances where the overlay is not applied
#' @return Studbook master table with overlay applied
#' @examples
#' 

apply_overlay_master <- function(studbook = studbook, 
                                 overlay_UID_to_apply = NULL, 
                                 add_IsHypothetical = TRUE){

  # pull out the regular table and overlay table

    reg <- studbook$Master

    if(add_IsHypothetical == TRUE)
      reg <- data.frame(reg, IsHypothetical=rep(0, nrow(studbook$Master)))
  
    ov <- studbook$OverlayMaster

  # subset the overlay table to which ever overlay to apply

    ov <- ov[which(ov$GeneratedGUID %in% overlay_UID_to_apply),]

    ov$studbookID <- as.character(ov$studbookID)
    ov$Sire <- as.character(ov$Sire)
    ov$Dam <- as.character(ov$Dam)

    reg$studbookID <- as.character(reg$studbookID)
    reg$Sire <- as.character(reg$Sire)
    reg$Dam <- as.character(reg$Dam)

  # go through each individual in the overlay

    for(i in 1:nrow(ov)){  

      if(length(which(as.character(reg$studbookID) == ov$studbookID[i])) > 0){

        # xx: the studbook data

          xx <- reg[which(as.character(reg$studbookID) == ov$studbookID[i]), ]    

        # yy: the overlay data

          yy <- ov[which(ov$studbookID == ov$studbookID[i]),]         
      
        # in the case that there are two entries for the same individual in 
        #  the overlay(s), take the first one, and print a warning

        if(nrow(yy) > 1){
          print(paste("Warning: multiple entries for individual ", 
                      ov$studbookID[i], 
                      " in the Overlay Master table.", 
                      " Using first entry as default.", sep = ""))
          yy <- yy[1, ]
        }

        if(is.na(yy$Sire) == F)
          xx$Sire <- yy$Sire
        if(is.na(yy$Dam) == F)
          xx$Dam <- yy$Dam
        if(is.na(yy$BirthDate) == F)
          xx$BirthDate <- yy$BirthDate
        if(is.na(yy$BDateEst) == F)
          xx$BDateEst <- yy$BDateEst
        if(is.na(yy$BirthType) == F)
          xx$BirthType <- yy$BirthType
        if(is.na(yy$IsHypothetical) == F & add_IsHypothetical == TRUE)
          xx$IsHypothetical <- yy$IsHypothetical
    
        reg[which(as.character(reg$studbookID) == ov$studbookID[i]),] <- xx
      }


      # if an individual isn't already in the studbook (they are most likely 
      #  hypothetical), then add them

        if(length(which(as.character(reg$studbookID) == ov$studbookID[i])) ==
           0){

          # grab the first row in mast as a template, replace all data with 
          #  NAs, then fill in stuff from the overlay

          xx <- reg[1,]
          xx[1, 1:ncol(xx)] <- NA
          yy <- ov[which(ov$studbookID == ov$studbookID[i]), ]
      
          xx$studbookID <- yy$studbookID    
          xx$Sire <- yy$Sire    
          xx$Dam <- yy$Dam
          xx$BirthDate <- yy$BirthDate  
          xx$BDateEst <- yy$BDateEst
          xx$BirthType <- yy$BirthType

          if(add_IsHypothetical == TRUE)
            xx$IsHypothetical <- yy$IsHypothetical

          reg <- rbind(reg, xx)
        }

    }

  return(reg)
}


#' Apply the event table overlay
#' 
#' @param studbook Studbook name
#' @param overlay_UID_to_apply ID of the overlay to apply 
#' @param add_IsHypothetical Logical of whether or not to add "IsHypothetical"
#'  with a value of 0 in instances where the overlay is not applied
#' @return Studbook event table with overlay applied
#' @examples
#' 

apply_overlay_event <- function(studbook = studbook,  
                                 overlay_UID_to_apply = NULL, 
                                 add_IsHypothetical = TRUE){

  # pull out the regular table and overlay table

    reg <- studbook$Event

    if(add_IsHypothetical == TRUE)
      reg <- data.frame(reg, IsHypothetical = rep(0, nrow(studbook$Event)))

    ov <- studbook$OverlayEvent

  # subset the overlay table to which ever overlay to apply

    ov <- ov[which(ov$GeneratedGUID %in% overlay_UID_to_apply), ]

    ov$studbookID <- as.character(ov$studbookID)
    reg$studbookID <- as.character(reg$studbookID)

  # split the data into "new" and "related" records

    ovR <- ov[-which(is.na(ov$RelatedRecord)),]
    ovN <- ov[which(is.na(ov$RelatedRecord)),]

  # only edit entries if there are "related" records

    if(nrow(ovR) > 0){

      for(i in 1:length(nrow(ovR))){

        # find the matching record

          spot <- which(as.character(reg$GeneratedGUID) == 
                        as.character(ovR$RelatedRecord[i]))

        # if it doesn't exist, bounce that error!

          if(length(spot) == 0)
            return(print(
                  "Event table overlay references record not in main table."))

        xx <- reg[spot,]
        yy <- ovR[i,]
  
        # if it's the wrong individual or event, bounce that error

          if(length(na.omit(yy$studbookID)) > 0){
           if(as.character(xx$studbookID) != as.character(yy$studbookID))
              return(print(
                     "Event table overlay references the wrong studbook ID."))
          }

          if(length(na.omit(yy$TranCode)) > 0){
            if(as.character(xx$TranCode) != as.character(yy$TranCode))
              return(print(
                     "Event table overlay references the wrong event type."))
          }

        if(is.na(yy$Location) == F)
          xx$Location <- yy$Location
        if(is.na(yy$TranDate) == F)
          xx$TranDate <- yy$TranDate
        if(is.na(yy$TDateEst) == F)
          xx$TDateEst <- yy$TDateEst
      
        reg[spot,] <- xx
      }
    }


  # only add the new entries if there are new entries to add!

    if(nrow(ovN) > 0){

      # grab the first row in reg as a template, replace all data with NAs, 
      #  then fill in stuff from the overlay

        xx <- reg[1,]
        xx[1,1:ncol(xx)] <- NA

        for(i in 1:nrow(ov)){  

          yy <- ov[i,]
          xx$studbookID <- yy$studbookID    
          xx$TranCode <- yy$TranCode    
          xx$Location <- yy$Location
          xx$TranDate <- yy$TranDate  
          xx$TDateEst <- yy$TDateEst

          if(add_IsHypothetical == TRUE)
            xx$IsHypothetical <- yy$IsHypothetical

          reg <- rbind(reg, xx)
        }

    }

  return(reg)
}


#' Apply the sex table overlay
#' 
#' @param studbook Studbook name
#' @param overlay_UID_to_apply ID of the overlay to apply 
#' @param add_IsHypothetical Logical of whether or not to add "IsHypothetical"
#'  with a value of 0 in instances where the overlay is not applied
#' @return Studbook sex table with overlay applied
#' @examples
#' 

apply_overlay_sex <- function(studbook = studbook,  
                                 overlay_UID_to_apply = NULL, 
                                 add_IsHypothetical = TRUE){

  # pull out the regular table and overlay table

    reg <- studbook$Sex

    if(add_IsHypothetical == TRUE)
      reg <- data.frame(reg, IsHypothetical = rep(0, nrow(studbook$Sex)))

    ov <- studbook$OverlaySex

  # subset the overlay table to which ever overlay to apply

    ov <- ov[which(ov$GeneratedGUID %in% overlay_UID_to_apply), ]

    ov$studbookID <- as.character(ov$studbookID)
    reg$studbookID <- as.character(reg$studbookID)
  
  # split the data into "new" and "related" records

    ovR <- ov[-which(is.na(ov$RelatedRecord)),]
    ovN <- ov[which(is.na(ov$RelatedRecord)),]

  # only edit entries if there are "related" records

    if(nrow(ovR) > 0){

      # find the matching record

        spot <- which(as.character(reg$GeneratedGUID) == 
                      as.character(ovR$RelatedRecord[i]))

      # if it doesn't exist, bounce that error!

        if(length(spot) == 0)
          return(print(
                 "Event table overlay references record not in main table."))

      xx <- reg[spot,]
      yy <- ovR[i,]
  
      # if it's the wrong individual bounce that error

        if(as.character(xx$studbookID) != as.character(yy$studbookID))
          return(print(
                 "Event table overlay references the wrong studbook ID."))

      if(is.na(yy$Sex) == F)
        xx$Sex <- yy$Sex
      if(is.na(yy$EventDate) == F)
        xx$EventDate <- yy$EventDate
      if(is.na(yy$EDateEst) == F)
        xx$EDateEst <- yy$EDateEst
      
      reg[spot,] <- xx

    }

  # only add the new entries if there are new entries to add!

  if(nrow(ovN) > 0){

    # grab the first row in reg as a template, replace all data with NAs,
    #  then fill in stuff from the overlay

      xx <- reg[1,]
      xx[1,1:ncol(xx)] <- NA

      for(i in 1:nrow(ov)){  

        yy <- ov[i,]
        xx$studbookID <- yy$studbookID    
        xx$Sex <- yy$Sex    
        xx$EventDate <- yy$EventDate  
        xx$EDateEst <- yy$EDateEst

        if(add_IsHypothetical == TRUE)
          xx$IsHypothetical <- yy$IsHypothetical

        reg <- rbind(reg, xx)
      }

  }

  return(reg)
}

