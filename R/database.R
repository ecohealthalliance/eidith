db_tables <- c("events", "animals", "specimens", "tests", "viruses", "test_specimen_ids", "status")

db_unique_indexes <- list(
  events = list("event_id"),
  animals = list("animal_id"),
  specimens = list("specimen_id"),
  tests = list("test_id"),
  viruses = list("virus_id"),
  test_specimen_ids = list()
)

db_other_indexes <- list(
  events = list("country"),
  animals = list("event_id"),
  specimens = list("animal_id", "specimen_id_name"),
  tests = list("specimen_id_name"),
  viruses = list("test_id"),
  test_specimen_ids = list("test_id", "specimen_id")
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
#' -  Fetches all tables from EIDITH (events, animals, specimens, tests, viruses, and test-specimen cross-references)
#' -  Cleans and pre-processes the data with [ed_process()].
#' -  Stores the data in an SQLite database in your [user data directory][rappdirs::user_data_dir].
#'
#' Each time it is run it clears out the previous data and downloads all the data,
#' as such it takes a few minutes.  In the future, we will allow for updating only with new and changed records.
#'
#' The function will prompt for username and password unless you have [cached your credentials][eidith_auth].
#'
#' @importFrom dplyr db_list_tables db_drop_table copy_to
#' @param verbose Show messages while in progress?
#' @export
download_db <- function(verbose=interactive()) {
  auth <- eidith_auth(verbose = verbose)
  if(verbose) message("Downloading and processing EIDITH data. This may take a few minutes.")
    tables <- lapply(endpoints, ed_get, postprocess=TRUE, verbose=FALSE, auth=auth)
  lapply(dplyr:: db_list_tables(eidith_db$con), function(x) {
    dplyr::db_drop_table(eidith_db$con, x)}
    )
  lapply(seq_along(tables), function(x) {
    dplyr::copy_to(eidith_db, tables[[x]], name=db_tables[x], temporary = FALSE,
                      unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
  })
  return()
}

#' #' @importFrom magrittr use_series
#' db_status <- function() {
#'   countries <- tbl(eidith_db, "events") %>% group_by(country) %>% summarise(n=n()) %>% collect() %>% use_series("country")
#'   n_countries <- length(countries)
#'   countries_str <- stri_paste(countries, collapse = ", ")
#'   last_db_update <- tbl(eidith_db, "events") %>% summarise(n = n()) %>% collect %>% use_series("n")
#' }
#'
#' update_db <- function() {
#'
#' }
#'
#' export_db <- function() {  #Exports the database file to new location.  options(eidith_db) should let you change it.
#'
#' }
