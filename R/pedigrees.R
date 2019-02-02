#' Create Pedigree
#' 
#' @details All dams and sires not present as individuals are replaced with NA
#'   entires. If an individual has one known parent and one NA parent, the NA
#'   parent is replaced with a made-up parent who is the offspring of two NAs
#'   and the made-up parent is added to the pedigree.
#' @param studbook Studbook
#' @param exclude Character vector of StudbookIDs to exclude from the pedigree
#' @return Pedgiree as a list of a data table and pedigree object.
#' 
#' @export 
#'
create_pedigree <- function(studbook = NULL, exclude = NULL){

  ind <- as.character(studbook$Master$StudbookID)
  dam <- as.character(studbook$Master$Dam)
  sire <- as.character(studbook$Master$Sire)  

  if(length(exclude) > 0){
    sire <- sire[-which(ind %in% exclude)]
    dam <- dam[-which(ind %in% exclude)]
    ind <- ind[-which(ind %in% exclude)]
  }
  dams_removed <- dam[which(!(dam %in% ind))]
  sires_removed <- sire[which(!(sire %in% ind))]
  dam[-which(dam %in% ind)] <- NA
  sire[-which(sire %in% ind)] <- NA

  sex <- rep(NA, length(ind))
  for(i in 1:length(ind)){
    xx <- studbook$Sex[which(studbook$Sex$StudbookID == ind[i]),]
    xx <- xx[which(xx$EventDate == max(xx$EventDate)), ]
    sex[i] <- as.character(xx$Sex)
  }
  sex[-which(sex %in% c("Male", "Female", "F", "M"))] <- 3
  sex[which(sex %in% c("M", "Male"))] <- 1
  sex[-which(sex %in% c("1", "3"))] <- 2	
  sex <- as.numeric(sex) 

  NAdam <- which(is.na(dam))
  NAsire <- which(is.na(sire))
  NADonly <- NAdam[-which(NAdam %in% NAsire)]
  NASonly <- NAsire[-which(NAsire %in% NAdam)]

  if(length(NADonly) > 0){
    dummy <- 99999
    dam[NADonly] <- paste(dummy, 1:length(NADonly), sep = "")	
    ind <- c(ind, paste(dummy, 1:(length(NADonly)), sep = ""))
    dam <- c(dam, rep(NA, length(NADonly)))
    sire <- c(sire, rep(NA, length(NADonly)))
    sex <- c(sex, rep(2, length(NADonly)))
  }
  if(length(NASonly) > 0){
    dummy <- 99999
    sire[NASonly] <- paste(dummy, 1:length(NASonly), sep = "")	
    ind <- c(ind, paste(dummy, 1:(length(NASonly)), sep = ""))
    dam <- c(dam, rep(NA, length(NASonly)))
    sire <- c(sire, rep(NA, length(NASonly)))
    sex <- c(sex, rep(1, length(NASonly)))
  }

  pedig_df <- data.frame(id = ind, mom = dam, dad = sire, sex = sex)
  pedig_po <- with(pedig_df, pedigree(id, dad, mom, sex))

  removed <- list(sires = sires_removed, dams = dams_removed)
  added <- ind[grep("99999", ind)]
  output <- list(pedig_df = pedig_df, pedig_po = pedig_po, removed = removed,
                 added = added, Master = studbook$Master)
  attr(output, "class") <- c("pedig_list", "list")
  attr(output, "hidden") <- c("pedig_po", "removed", "added", "Master")
  output
}

print.pedig_list <- function(x, ...){
  hid <- attr(x, "hidden")
  notHid <- !names(x) %in% hid
  print(x[notHid])
}

#' Calculate the kinship of a pedig_list object
#' 
#' @param id pedig_list object
#' @param ... optional arguments
#' @param remove_hypotheticals logical indicating if hypotheticals should
#'   be removed from the output
#' @return kinship matrix
#'
#' @export 
#'
kinship.pedig_list <- function(id, ..., remove_hypotheticals = TRUE){
  kin <- kinship(id$pedig_po, ...)
  if(remove_hypotheticals){
    if(length(which(names(id$Master) == "IsHypothetical")) == 1){
      keepers <- (id$Master)$IsHypothetical == 0
      kin <- kin[keepers, keepers]
    }
  }
  kin
}

#' Calculate the inbreeding of a pedig_list object
#' 
#' @param pedig_list pedig_list object
#' @param remove_hypotheticals logical indicating if hypotheticals should
#'   be removed from the output
#' @return data frame with individual and parental ids and inbreeding values
#'
#' @export 
#'
inbreeding <- function(pedig_list, remove_hypotheticals = TRUE){
  fped <- fixPedigree(pedig_list$pedig_df)
  out <- data.frame(fped, Fvalue = calcInbreeding(fped))
  if(remove_hypotheticals){
    if(length(which(names(pedig_list$Master) == "IsHypothetical")) == 1){
      out <- out[(pedig_list$Master)$IsHypothetical == 0, ]
    }
  }
  out[order(as.numeric(as.character(out$id))), ]
}


