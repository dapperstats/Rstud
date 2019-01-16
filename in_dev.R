devtools::load_all()

examplesb <- load_poplink_studbook(db_name = "Example", 
                                  db_tables = c("Master", "Event"), 
                                  overlay = TRUE, udf = TRUE, 
                                  verbose = TRUE)
  create_institution_window("AZA")
  create_date_window()
  


# to do
#
# bring in code from gk and rw
#
# low priority rn:
# tidy up the overlay script
# add population by institution name to vortex_sb (code is with old scripts/rws)