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

#' @importFrom dplyr db_list_tables db_drop_table copy_to
#' @export
download_db <- function(verbose=interactive()) {
  auth <- eidith_auth(verbose = verbose)
  if(verbose) message("Downloading and processing EIDITH data. This may take a few minutes.")
  #with_envvar(c("EIDITH_USERNAME"=auth[1], "EIDITH_PASSWORD"=auth[2]), {
    tables <- lapply(endpoints, ed_get, postprocess=TRUE, verbose=FALSE, auth=auth)
#  }, action="replace")
  lapply(dplyr:: db_list_tables(eidith_db$con), function(x) {
    dplyr::db_drop_table(eidith_db$con, x)}
    )
  lapply(seq_along(tables), function(x) {
    dplyr::copy_to(eidith_db, tables[[x]], name=db_tables[x], temporary = FALSE,
                      unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
  })
  return()
}

update_db <- function() {

}

export_db <- function() {  #Exports the database file to new location.  options(eidith_db) should let you change it.

}
