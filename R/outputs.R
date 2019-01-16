#' Create a studbook style input for Vortex modeling
#'
#' @details Vortex doesn't handle unknown sexes properly, so we assume all 
#'  unknown sex indivuals are males.
#'
#' @param studbook studbook
#' @param pedig_list pedig_list
#' @param events_list events tables list
#' @param mates optional table for pre-assigning mates
#' @param write_out logical indicating if the table should be written out
#' @return studbook style input table for vortex modeling
#' @export
#'
vortex_sb <- function(studbook, pedig_list, events_list, mates = NULL,
                      write_out = TRUE){

  pedig <- pedig_list$pedig_df
  ind <- as.character(pedig$id)
  sire <- as.character(pedig$dad) 
  dam <- as.character(pedig$mom) 
  sex <- pedig$sex 
  sex2 <- sex
  sex2[which(sex2 %in% c(1, 3))] <- "Male"
  sex2[which(sex2 == 2)] <- "Female"

  nind <- length(ind)
  alive <- rep(0, nind)
  age <- rep(-1, nind)
  population <- rep(0, nind)
  currd <- as.Date(studbook$DatabaseDetails$CurrentnessDate)

  for(i in 1:nind){
    indi <- events_list$events[i, ]

    if(all(c(!is.na(indi$birthday), is.na(c(indi$deathday, indi$ltfday))))){
      alive[i] <- 1
      birthd <- as.Date(as.character(indi$birthday))
      age[i] <- as.numeric(floor(difftime(currd, birthd, unit = "days")/365))
      population[i] <- 1
    }
  }
  mate <- rep(-1, nind)
  if(!is.null(mates)){
    colnames(mates) <- tolower(colnames(mates))
    for(i in 1:nrow(mates)){
      dd <- mates$dam[i]
      ss <- mates$sire[i]
      mate[which(ind == dd)] <- ss
    }
  }

  out <- data.frame(ID = ind, Sire = sire, Dam = dam, Sex = sex2, 
                    Selected = alive, Alive = alive, Population = population, 
                    AGE = age, MATE = mate)
  if(write_out){
    write.table(out, "VortexInput.txt", sep = "\t", row.names = FALSE,
                quote = FALSE)
  }
  out
}