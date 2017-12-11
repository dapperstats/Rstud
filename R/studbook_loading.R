#' Load PopLink Studbook 
#' 
#' @param db_name Database name
#' @param db_tables Names of database tables to retrieve
#' @param overlay Logical of whether or not to retrieve the overlay
#' @param udf Logical of whether or not to retrieve the user defined field
#' @param verbose Logical of whether or not to print database and 
#'   overlay details
#' @return Studbook as a named listed of database tables
#' @examples
#' 
#' 

load_poplink_studbook <- function(db_name = NULL, 
                                  db_tables = c("Master", "Event"), 
                                  overlay = TRUE, udf = TRUE, 
                                  verbose = FALSE){

  if(length(db_name) == 0)
    return(print("No studbook name given!"))
  
  # connect to the studbook

    # determine the name of the computer

      CompName <- Sys.info()['nodename']

    # paste together the calls to the ODBC driver connection

      xx <- paste("driver={SQL Server};server=", 
                  CompName, "\\SQLEXPRESS;database=", db_name, 
                  ";trusted_connection=true", sep = "")

    # set up the connection
 
      dbhandle <- RODBC::odbcDriverConnect(xx)

  # add the udf if requested

    if(udf == TRUE)
      db_tables <- c(db_tables, "UserDefinedField", "UserDefinedFieldValue")

  # grab all of the main tables, save them as named elements in a list

    output <- vector("list", length(db_tables))

    for(i in 1:length(db_tables)){
      output[[i]] <- RODBC::sqlQuery(dbhandle, 
                                     paste("select * from ", db_tables[i], 
                                           sep = ""))  
    }

    # will be used to name the list elements

      nn <- db_tables  

  # add in the overlay tables if wanted (by default they are, 
  #  even if they don't exist!)

    if(overlay == TRUE){

      ndbt <- length(db_tables)
      for(i in 1:ndbt){
        output[[ndbt + i]] <- RODBC::sqlQuery(dbhandle, 
                                              paste("select * from overlay",
                                                    db_tables[i], sep=""))
      }

      lo <- length(output)
      output[[lo + 1]] <- RODBC::sqlQuery(dbhandle, 
                                          "select * from overlayInformation")

      nn <- c(nn, paste("overlay", db_tables, sep = ""), "overlayInformation")

    }

  # collect details

    Details <- RODBC::sqlQuery(dbhandle, "select * from Overview")
    output[[length(output) + 1]] <- Details  
    names(output) <- c(nn, "DatabaseDetails")

  # print the details about the database if requested

    if(verbose == TRUE){
      print(paste(Details$CommonName, " studbook version ", 
                  Details$Version, ", current to ", Details$CurrentnessDate, 
                  " and kept by ", Details$StudbookKeeper, " at ",
                  Details$StudbookKeeperInstitution, sep = ""))
    }


  # if overlays were requested and verbose is toggled on, examine the
  #  overlays (using overlayExamine) and print the simple results
  
  if(overlay == TRUE  & verbose == TRUE){
    if(nrow(output$overlayInformation) > 0){
      if(nrow(output$overlayMaster) == 0){
        print("Only empty overlays present.")
      } else {
        print(examine_overlays(output)$overlaySummary)
      }
    }
  }

  # close the ODBC handle

    RODBC::odbcCloseAll()

  # return the studbook 

    return(output)

}


#' Determine how many overlays exist and parse the overlay tables 
#' 
#' @param studbook Studbook name
#' @param verbose Logical about whether or not to print 
#' @param full_return Logical about whether to return the overlay or 
#'  just details
#' @return List of overlay tables, split by overlay.
#' @examples
#' 

examine_overlays <- function(studbook = NULL, verbose = FALSE, 
                             full_return = FALSE){

  if(length(studbook) == 0)
    return(print("No studbook given!"))

  # find which elements in the input are overlay components 

    OLcomponents <- grep("Overlay", names(studbook))

  # if there aren't any overlay components, say so and end the function

    if(length(OLcomponents) == 0)
      return(print("There are no overlay components in the studbook."))

  # how many overlays exist?

    nOL <- nrow(studbook$OverlayInformation)

  # if no overlays exist, say so and end function

    if(nOL == 0){
      if(verbose == TRUE)
        print("There are no overlays.")
      return()
    }  

  # if overlays exist, say how many

    if(verbose == TRUE)
      print(paste("Number of overlays in the studbook: ", nOL, sep=""))
  
  # collect the summary information 

    OLsummary <- studbook$OverlayInformation
    OLsummary <- OLsummary[ , which(colnames(OLsummary) %in% 
                                   c("Name","Description", "DateCreated", 
                                     "DateEdited", "UserCreated", 
                                     "UserEdited"))]

  # create the output and populate the first element with the summary

    output <- vector("list", 1)
    output[[1]] <- OLsummary
    names(output) <- "OverlaySummary"

  # if full overlays are requested

    if(full_return == TRUE){

      overlayUIDs <- (studbook$OverlayInformation)$GeneratedGUID

      # create a list of lists for the overlays  
      #  (each overlay will be a list of its components, 
      #   and then there will be a list of the overlay lists)

      OLlist <- vector("list", length=nOL)
      names(OLlist) <- OLsummary$Name

      for(i in 1:nOL){

        sub_OLlist <- vector("list", length = length(OLcomponents))
        names(sub_OLlist) <- names(studbook)[OLcomponents]
        entries <- rep(0, length(sub_OLlist))  
    
        for(j in 1:length(sub_OLlist)){
          ff <- studbook[[OLcomponents[j]]]
          ff <- ff[which(as.character(ff$GeneratedGUID) == 
                         as.character((
                           studbook$OverlayInformation)$GeneratedGUID[i])), ]
          ff <- ff[ , -which(colnames(ff) %in% c("UniqueID", "GeneratedGUID",
                                                 "IndividualGUID"))]
          sub_OLlist[[j]] <- ff
          entries[j] <- nrow(ff)
        }

        tablesWith <- names(sub_OLlist)[which(entries != 0)]
      
        sub_OLlist2 <- vector("list", length = length(tablesWith))
      
        for(j in 1:length(tablesWith)){
          sub_OLlist2[[j]] <- sub_OLlist[[which(names(sub_OLlist) %in% 
                                                tablesWith[j])]]
        }

        names(sub_OLlist2) <- names(sub_OLlist)[which(names(sub_OLlist) %in% 
                                                      tablesWith)]
        output[[length(output) + 1]] <- sub_OLlist2

      }

      names(output)[2:length(output)] <- as.character(OLsummary$Name)
    }
    
  return(output)

}

