
#' Determine how many UDFs exist, print, and return list
#' 
#' @param studbook Studbook name
#' @param verbose Logical about whether or not to print the UDFs
#' @return List of UDFs, each containing its levels.
#' 
#' @export 
#'
examine_UDFs <- function(studbook = NULL, verbose = TRUE){

  UDFs <- as.character((studbook$UserDefinedField)$FieldName)

  if(length(which(UDFs != "Studbook ID")) == 0){
    stop("There are no UDF data in the Studbook you gave the function.")
  }

  UDFs <- UDFs[which(UDFs %in% "Studbook ID" == FALSE)]
  nUDFs <- length(UDFs)
  output <- vector("list", length = nUDFs)
  names(output) <- UDFs
  for(i in 1:nUDFs){
		
    spot <- which((studbook$UserDefinedField)$FieldName == UDFs[i])
    levs <- (studbook$UserDefinedFieldValue)$FieldUniqueID == spot
    ulevs <- as.character(unique(studbook$UserDefinedFieldValue$Value[levs]))

    if(verbose){
      msg_UDF <- paste0("UDF: [", UDFs[i], "]")
      txt_levs <- paste0(ulevs, collapse = "], [")
      msg_levs <- paste0(" with levels: [", txt_levs, "]")
      msg <- paste0(msg_UDF, msg_levs)
      message(msg)
    }
    output[[i]] <- ulevs
  }
  output
}
	


#' Apply Selected UDF (and Optionally Restrict Individuals)
#' 
#' @param studbook Studbook name
#' @param udf Name of UDF to apply
#' @param levels Vector of levels of the UDF
#' @param retain Logical of whether levels should be kept (TRUE) or 
#'  dropped (FALS)
#' @param sb_ids Character vector of studbook IDs to further limit the output
#' @return Studbook IDs of all individuals that match the levels of the UDF
#'  as requested, with additional optional restrictions
#' 
#' @export 
#'
apply_UDF <- function(studbook = NULL, udf = NULL, levels = NULL, 
                      retain = TRUE, sb_ids = NULL){

  if(length(which(names(studbook) == "UserDefinedField")) == 0){
    stop("There is not a UDF definition in the Studbook provided.")
  }
  if(length(which(names(studbook) == "UserDefinedFieldValue")) == 0){
    stop("There is not a UDF in the Studbook provided.")
  }

  UDFs <- as.character((studbook$UserDefinedField)$FieldName)
  whichFields <- which(UDFs %in% udf == TRUE)
  if(length(whichFields) == 0){
    stop("That UDF does not exist.")
  }

  spots <- which(studbook$UserDefinedFieldValue$FieldUniqueID == whichFields)
  temp1 <- studbook$UserDefinedFieldValue[spots, ]
  indivs <- temp1$StudbookID[which(temp1$Value %in% levels == retain)]
  if(length(sb_ids) == 0){
    return(indivs)
  }
  sb_ids %in% indivs
}	
