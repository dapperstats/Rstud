
#' Determine how many UDFs exist, print, and return list
#' 
#' @param studbook Studbook name
#' @param verbose Logical about whether or not to print the UDFs
#' @return List of overlay tables, split by overlay.
#' @examples
#' 

examine_udfs <- function(studbook = NULL, verbose = TRUE){

  UDFs <- as.character((studbook$UserDefinedField)$FieldName)

  if(length(which(UDFs != "Studbook ID")) == 0)
    return(print(
            "There are no UDF data in the Studbook you gave the function."))

  UDFs <- UDFs[which(UDFs %in% "Studbook ID" == FALSE)]

  for(i in 1:length(UDFs)){
		
    spot <- which(as.character((studbook$UserDefinedField)$FieldName) == 
                  UDFs[i])
    levs <- as.character(unique((studbook$UserDefinedFieldValue)$Value[
              (studbook$UserDefinedFieldValue)$FieldUniqueID == spot]))

    if(verbose == TRUE){
      print(paste("UDF: [", UDFs[i], "] with levels: [", 
            paste0(levs, collapse = "], ["), "]", sep = ""))
    }
  }
  return(UDFs)
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
#' @examples
#' 
#' 


apply_UDF <- function(studbook = NULL, udf = NULL, levels = NULL, 
                      retain = TRUE, sb_ids = NULL){

  if(length(which(names(studbook) == "UserDefinedField")) == 0){
    return(print(
     "There is not a UDF explanation in the Studbook you gave the function."))
  }

  if(length(which(names(studbook) == "UserDefinedFieldValue")) == 0){
    return(print(
     "There are not UDF data in the Studbook you gave the function."))
  }

  UDFs <- as.character((studbook$UserDefinedField)$FieldName)
  whichFields <- which(UDFs %in% udf == TRUE)

  if(length(whichFields) == 0)
    return(print("That UDF does not exist."))

  temp1 <- studbook$UserDefinedFieldValue[which((
             studbook$UserDefinedFieldValue)$FieldUniqueID == whichFields),]

  indivs <- temp1$StudbookID[which(temp1$Value %in% levels == retain)]
	
  if(length(sb_ids) == 0)
    return(indivs)

  InOut <- sb_ids %in% indivs
  return(InOut)
	
}	
