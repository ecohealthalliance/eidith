db_tables <- c("events", "animals", "specimens", "tests", "viruses",
               "test_specimen_ids", "status")

db2_tables <- c("events_2", "animals_2", "specimens_2", "animal_production_2", "crop_production_2",
                "dwellings_2", "human_2", "market_value_chain_2", "natural_areas_2", "zoo_sanctuary_2",
                "wildlife_restaurant_2", "tests_2", "test_data_interpreted_2", "test_data_serology_2",
                "human_animal_production_2", "extractive_industry_2", "human_crop_production_2",
                "human_extractive_industry_2", "human_hospital_worker_2", "human_hunter_2",
                "human_market_2", "human_restaurant_2", "human_sick_person_2", "human_temporary_settlements_2",
                "human_zoo_2", "behavioral_2", "training_2",
                "human_ehp_2",  "human_animal_production_ehp_2", "human_hunter_ehp_2"
                )

p1_table_names <- list(
  Event = "events",
  Animal = "animals",
  Specimen = "specimens",
  Test = "tests",
  Virus = "viruses",
  TestIDSpecimenID = "test_specimen_ids"
)

p2_table_names <- list(
  Event = "events_2",
  Animal = "animals_2",
  Specimen = "specimens_2",
  AnimalProduction = "animal_production_2",
  CropProduction = "crop_production_2",
  Dwellings = "dwellings_2",
  NaturalAreas = "natural_areas_2",
  MarketValueChain = "market_value_chain_2",
  ZooSanctuary = "zoo_sanctuary_2",
  WildlifeRestaurant = "wildlife_restaurant_2",
  Human = "human_2",
  HumanEHP = "human_ehp_2",
  Test = "tests_2",
  TestDataInterpreted = "test_data_interpreted_2",
  TestDataSerology = "test_data_serology_2",
  HumanAnimalProduction = "human_animal_production_2",
  HumanAnimalProductionEHP = "human_animal_production_ehp_2",
  ExtractiveIndustry = "extractive_industry_2",
  HumanCropProduction = "human_crop_production_2",
  HumanExtractiveIndustry = "human_extractive_industry_2",
  HumanHospitalWorker = "human_hospital_worker_2",
  HumanHunter = "human_hunter_2",
  HumanHunterEHP = "human_hunter_ehp_2",
  HumanMarket = "human_market_2",
  HumanRestaurant = "human_restaurant_2",
  HumanSickPerson = "human_sick_person_2",
  HumanTemporarySettlements = "human_temporary_settlements_2",
  HumanZoo = "human_zoo_2",
  Behavioral = "behavioral_2",
  Training = "training_2"
)


db_unique_indexes <- list(
  events = list("event_id"),
  animals = list("animal_id"),
  specimens = list("specimen_id"),
  tests = list("test_id"),
  viruses = list("sequence_id"),
  test_specimen_ids = list(),
  events_2 = list("integer_id"),
  animals_2 = list("integer_id"),
  specimens_2 = list("integer_id"),
  animal_production_2 = list("integer_id"),
  crop_production_2 = list("integer_id"),
  dwellings_2 = list("integer_id"),
  market_value_chain_2 = list("integer_id"),
  natural_areas_2 = list("integer_id"),
  zoo_sanctuary_2 = list("integer_id"),
  wildlife_restaurant_2 = list("integer_id"),
  human_2 = list("integer_id"),
  human_ehp_2 = list("integer_id"),
  tests_2 = list("integer_id"),
  test_data_interpreted_2 = list("integer_id"),
  test_data_serology_2 = list("integer_id"),
  human_animal_production_2 = list("integer_id"),
  human_animal_production_ehp_2 = list("integer_id"),
  extractive_industry_2 = list("integer_id"),
  human_crop_production_2 = list("integer_id"),
  human_extractive_industry_2 = list("integer_id"),
  human_hospital_worker_2 = list("integer_id"),
  human_hunter_2 = list("integer_id"),
  human_hunter_ehp_2 = list("integer_id"),
  human_market_2 = list("integer_id"),
  human_restaurant_2 = list("integer_id"),
  human_sick_person_2 = list("integer_id"),
  human_temporary_settlements_2 = list("integer_id"),
  human_zoo_2 = list("integer_id"),
  behavioral_2 = list("integer_id"),
  training_2 = list("integer_id")
  )

db_other_indexes <- list(
  events = list("country"),
  animals = list("event_id"),
  specimens = list("animal_id", "specimen_id_name"),
  tests = list("specimen_id_names"),
  viruses = list("test_id"),
  test_specimen_ids = list("test_id", "specimen_id"),
  events_2 = list("event_name"),
  animals_2 = list("animal_id"),
  specimens_2 = list("specimen_id"),
  animal_production_2 = list("event_name"),
  crop_production_2 = list("event_name"),
  dwellings_2 = list("event_name"),
  market_value_chain_2 = list("event_name"),
  natural_areas_2 = list("event_name"),
  zoo_sanctuary_2 = list("event_name"),
  wildlife_restaurant_2 = list("event_name"),
  human_2 = list("participant_id"),
  human_ehp_2 = list("participant_id"),
  tests_2 = list("gains4_test_id"),
  test_data_interpreted_2 = list("gains4_test_id"),
  test_data_serology_2 = list("gains4_test_id"),
  human_animal_production_2 = list("participant_id"),
  human_animal_production_ehp_2 = list("participant_id"),
  extractive_industry_2 = list("event_name"),
  human_crop_production_2 = list("participant_id"),
  human_extractive_industry_2 = list("participant_id"),
  human_hospital_worker_2 = list("participant_id"),
  human_hunter_2 = list("participant_id"),
  human_hunter_ehp_2 = list("participant_id"),
  human_market_2 = list("participant_id"),
  human_restaurant_2 = list("participant_id"),
  human_sick_person_2 = list("participant_id"),
  human_temporary_settlements_2 = list("participant_id"),
  human_zoo_2 = list("participant_id"),
  behavioral_2 = list("transcript_id"),
  training_2 = list("trainee")
)



#' Download EIDITH data to local storage
#'
#' This function fetches all available tables from the EIDITH database and
#' caches the data locally for further use with the [table functions][ed_table()].
#' The database persists between R sessions, so you only need to run this when
#' you want to update data.  You should also run it when updating to new
#' versions of the **eidith** package.
#'
#' This function does a few things:
#' -  Fetches all tables from both EIDITH PREDICT-1 and PREDICT-2 databases.
#' -  Cleans and pre-processes the data with [ed_process()].
#' -  Stores the data in an SQLite database in your [user data directory][rappdirs::user_data_dir].
#'
#' Each time this function is run it checks for updated metadata about database tables,
#' then downloads each table into a temporary database and makes sure that the
#' structure of these tables is as expected. Only well-formed tables will be copied
#' into the user's permanent database. If a downloaded table is missing fields, has
#' extra fields, or is otherwise corrupted, the user will receive an error message.
#' In the future, we will allow for updating only with new and changed records.
#'
#' The function will prompt for username and password unless you have [cached your credentials][ed_auth].
#'
#' @importFrom dplyr db_list_tables db_drop_table group_by %>% pull
#' @importFrom RSQLite sqliteCopyDatabase
#' @importFrom DBI dbWriteTable
#' @importFrom purrr safely
#' @param p1_tables Which PREDICT-1 tables to download
#' @param p2_tables which PREDICT-2 tables to download
#' @param verbose Show messages while in progress?
#' @seealso [ed_db_status()], [ed_db_updates()], [ed_db_export()]
#' @export
ed_db_download <- function(p1_tables = p1_api_endpoints(), p2_tables = p2_api_endpoints(),
                           country = NULL, p1_data=FALSE, verbose=interactive()) {
  auth <- ed_auth(verbose = verbose)
  if (verbose) message("Downloading and processing EIDITH data. This may take some time.")

  eidith_disconnect(.eidith_env)
  ed_db_delete(temp_sql_path())

  #P1
  lapply(p1_tables, function(x) {
    tb <- ed_get(x, postprocess=TRUE, verbose=verbose, auth=auth)
    dplyr::copy_to(eidith_db(temp_sql_path()), tb, name=p1_table_names[[x]], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[p1_table_names[[x]]]], indexes = db_other_indexes[[p1_table_names[[x]]]])
    eidith_disconnect(.eidith_env)
    rm(tb);
    gc(verbose=FALSE)
  })
  # P2
  lapply(p2_tables, function(x) {
    tb <- ed2_get(x, country=country, p1_data=p1_data, postprocess=TRUE, verbose=verbose, auth=auth)
    if(any(typeof(tb) != "list")) return(invisible(0)) #escaping if there is an error with the download
    tb$integer_id <- seq_len(nrow(tb))
    dplyr::copy_to(eidith_db(temp_sql_path()), tb, name=p2_table_names[[x]], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[p2_table_names[[x]]]], indexes = db_other_indexes[[p2_table_names[[x]]]])
    eidith_disconnect(.eidith_env)
    rm(tb);
    gc(verbose=FALSE)
  })

  # if(nrow(p2_key_errors) > 0){
  #   dbWriteTable(eidith_db(temp_sql_path()), p2_key_errors, name = "p2_unique_id_errors", append = TRUE)
  # }
  #Check that requested tables have downloaded:
  p1_dls <- unname(sapply(p1_tables, function(x) p1_table_names[[x]]))
  p2_dls <- unname(sapply(p2_tables, function(x) p2_table_names[[x]]))

  downloaded_tables <- unlist(c(p1_dls, p2_dls))
  if(!(all(downloaded_tables %in% db_list_tables(eidith_db(temp_sql_path()))))) {
    downloaded_tables <- downloaded_tables[which(downloaded_tables %in% db_list_tables(eidith_db(temp_sql_path())))]
  }
  ed2_meta <- ed2_metadata()
  if(!all(sapply(downloaded_tables, function(x) ed_db_field_check(x, temp_sql_path(), ed2_meta)))){
    downloaded_tables <- downloaded_tables[which(sapply(downloaded_tables, function(x) ed_db_field_check(x, temp_sql_path(), ed2_meta)))]
  }

  if(verbose) {
      if(length(downloaded_tables) == length(c(p1_dls, p2_dls))){
      cat_line("All database tables have successfully downloaded!")
      } else if(length(downloaded_tables < length(c(p1_dls, p2_dls)))){
        cat_line("Problems with remote EIDITH database / API prevented some tables from downloading.")
        cat_line("If problems persist see ?ed_contact for support.")
        cat_line("")
      }
    else{
      cat_line("Problems with remote EIDITH database / API prevented all tables from downloading.")
      cat_line("If problems persist see ?ed_contact for support.")
      cat_line("")
        return(invisible(0))
    }
}
  lapply(downloaded_tables, function(x){
      temp_tbl <- dbReadTable(eidith_db(temp_sql_path()), x)
      dbWriteTable(eidith_db(), value = temp_tbl, name = x, overwrite = TRUE)
    })

    # if("p2_unique_id_errors" %in% dbListTables(eidith_db(temp_sql_path()))){
    #   dbWriteTable(eidith_db(), value = dbReadTable(eidith_db(temp_sql_path()), "p2_unique_id_errors"),
    #               name = "p2_unique_id_errors", append = TRUE)
    # }

    # creating status
    status_df <- data_frame(unique_id = seq_along(downloaded_tables), t_name = unlist(downloaded_tables), last_download = as.character(Sys.time()))

    if("status" %in% db_list_tables(eidith_db())){
      dbWriteTable(eidith_db(), name = "status", value = status_df, append = TRUE, row.names = FALSE)
    }else{
      dbWriteTable(eidith_db(), value = status_df,
                   name="status", row.names = FALSE)
    }
    suppressWarnings(file.remove(temp_sql_path()))
    cat(ed_db_presence())
    cat(ed_db_check_status())
  ed_db_delete(temp_sql_path())
  return(invisible(0))
  }



#' Export the local EIDITH database to a file
#'
#' This function allows you to export the local EIDITH database to a file that
#' can then be used by others.  The database is in [SQLite](https://sqlite.org/) format.
#' @param filename The filename to export to. We suggest something ending in `.sqlite`.
#' @param ... Other options passed to [file.copy()]
#' @seealso  [ed_db_status()], [ed_db_updates()], [ed_db_export()]
#' @examples
#' \dontrun{
#'   #Here's an example of how to export and then use the exported database
#'
#'   ed_db_export("mydb.sqlite")
#'   options(ed_sql_path = "mydb.sqlite") # This switches to working with the exported database
#'   ed_db_status()  #get status of the current (exported) database
#' }
#' @export
#' @importFrom RSQLite sqliteCopyDatabase
ed_db_export <- function(filename, ...) {  #Exports the database file to new location.  options(eidith_db) should let you change it.
  sqliteCopyDatabase(eidith_db(), filename, ...)
}

#
# ed_db_updates <- function(path = NULL) {    # NEEDS TO BE RE-WORKED
#   last_download <- stri_replace_first_fixed(
#     collect(tbl(eidith_db(path), "status"))$last_download,
#     " ", "T")
#   auth <- ed_auth()
#   check_at <- p1_api_endpoints()[p1_api_endpoints() !="TestIDSpecimenID"]
#   new_rows <- lapply(check_at, function(endpoint) {
#     newdat <- ed_get(endpoint = endpoint, verbose = FALSE,
#                      lmdate_from = last_download, postprocess = FALSE, auth = auth)
#     nrow(newdat)
#   })
#   is_new_data <- as.logical(unlist(new_rows))
#   names(is_new_data) <- check_at
#   if(all(!is_new_data)) {
#     message("No new data since ", last_download, ".")
#   } else {
#     message("New data at endpoints: [",
#             paste0(check_at[is_new_data], collapse=","),
#             "]. Use ed_db_download() to update.")
#   }
#   return(is_new_data)
# }


#' Delete the local EIDITH database
#'
#' This function allows you to delete the local SQLite EIDITH database.
#'
#' @param path Path to locate the database if it is not in its default location.
#' @param verbose print messages?
#' @export

ed_db_delete <- function(path = NULL, verbose = TRUE) {
  suppressMessages({
    if (is.null(path)) {
      try(eidith_disconnect(.eidith_env), silent = TRUE)
      path <- default_sql_path()
      if (file.exists(path))
        status <- file.remove(default_sql_path())
      else {
        if (verbose) cat_line("No EIDITH database found.")
        status <- FALSE
      }
      .eidith_env$db <- NULL
      if (status == TRUE) {
        if (verbose) cat_line("Local EIDITH database successfully deleted.")
        invisible(gc(verbose = FALSE))
      } else if (file.exists(path)) {
        if (verbose) cat_line("There were problems deleting local EIDITH database, check that you have appropriate access.")
        invisible(gc(verbose = FALSE))
      }
      assign("db", NULL, envir = .eidith_env)
    } else {
      try(eidith_disconnect(.eidith_env), silent = TRUE)
      if (file.exists(path))
        status <- file.remove(path)
      invisible(gc(verbose = FALSE))
      assign("db", NULL, envir = .eidith_env)
    }
  })
}




