check_processing<- function(specimens, tests){
  cale_tests <- pp_Test(tests)
  cale_specimens <- pp_Specimen(specimens)
  cale_tests$Specimens <- str_split(cale_tests$CorrectedSpecimenNames, ",")
  cale_all_specs <- unlist(cale_tests$Specimens)
  cale_all_specs <- unique(cale_all_specs)
  missing_cale <- cale_all_specs[which(cale_all_specs %in% cale_specimens$CorrectedSpecimenIDUnique == FALSE)]
  missing_cale
}








