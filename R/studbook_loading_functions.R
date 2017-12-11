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



