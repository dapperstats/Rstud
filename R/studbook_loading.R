#' Load PopLink Studbook 
#' 
#' @param db_name Database name
#' @param db_tables Names of database tables to retrieve
#' @param overlay Logical of whether or not to retrieve the overlay
#' @param udf Logical of whether or not to retrieve the user defined field
#' @param verbose Logical of whether or not to print database and 
#'   overlay details
#' @return Studbook as a named listed of database tables
#' 
#' @export 
#'
load_poplink_studbook <- function(db_name = NULL, 
                                  db_tables = c("Master", "Event"), 
                                  overlay = TRUE, udf = TRUE, 
                                  verbose = FALSE){

  if(length(db_name) == 0){
    stop("No studbook name given")
  }  

  driver <- "driver={SQL Server}"
  server <- paste0("server=", Sys.info()["nodename"], "\\SQLEXPRESS")
  dbase <- paste0("database=", db_name)
  tcon <- "trusted_connection=true"
  con <- paste(driver, server, dbase, tcon, sep = ";")
  dbhandle <- odbcDriverConnect(con)

  if(udf){
    db_tables <- c(db_tables, "UserDefinedField", "UserDefinedFieldValue")
  }

  db_names <- db_tables 
  ndbt <- length(db_tables)

  output <- vector("list", length(db_tables))
  for(i in 1:ndbt){
    query <- paste0("select * from ", db_tables[i])
    output[[i]] <- sqlQuery(dbhandle, query)  
  }
   
  if(overlay){

    for(i in 1:ndbt){
      query <- paste0("select * from overlay", db_tables[i])
      output[[ndbt + i]] <- sqlQuery(dbhandle, query)
    }

    lo <- length(output)
    output[[lo + 1]] <- sqlQuery(dbhandle, "select * from overlayInformation")
    dbo_names <- c(paste0("overlay", db_tables), "overlayInformation")
    db_names <- c(db_names, dbo_names)

  }

  Details <- sqlQuery(dbhandle, "select * from Overview")
  lo <- length(output)
  output[[lo + 1]] <- Details  
  names(output) <- c(db_names, "DatabaseDetails")

  if(verbose){
    name <- Details$CommonName
    vers <- paste0("studbook version ", Details$Version)
    curr <- paste0("current to ", Details$CurrentnessDate)
    keep <- paste0("and kept by ", Details$StudbookKeeper)
    keepin <- paste0("at ", Details$StudbookKeeperInstitution)
    msg <- paste(name, vers, curr, keep, keepin, sep = " ")
    message(msg)
  }

  if(overlay & verbose){
    if(nrow(output$overlayInformation) >= 0){
      if(nrow(output$overlayMaster) == 0){
        message("Only empty overlays present.")
      } else {
        message(examine_overlays(output)$overlaySummary)
      }
    }
  }

  odbcCloseAll()

  output

}



