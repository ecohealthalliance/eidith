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
#' The function will prompt for username and password unless you have [cached your credentials][ed_auth].
#'
#' @importFrom dplyr db_list_tables db_drop_table copy_to
#' @param verbose Show messages while in progress?
#' @export
ed_db_download <- function(verbose=interactive()) {
  auth <- ed_auth(verbose = verbose)
  if(verbose) message("Downloading and processing EIDITH data. This may take a few minutes.")
  tables <- lapply(endpoints, ed_get, postprocess=TRUE, verbose=verbose, auth=auth)
  lapply(dplyr:: db_list_tables(eidith_db()$con), function(x) {
    dplyr::db_drop_table(eidith_db()$con, x)}
  )
  lapply(seq_along(tables), function(x) {
    dplyr::copy_to(eidith_db(), tables[[x]], name=db_tables[x], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
  })
  dplyr::copy_to(eidith_db(), data.frame(last_download=as.character(Sys.time())),
                 name="status", temporary=FALSE)
  if(verbose) {
    message("Database updated!")
    message(db_status()[["status_msg"]])
  }
}

#' @importFrom magrittr use_series
#' @importFrom purrr map_chr
#' @importFrom dplyr tbl group_by_ summarise_ collect lst db_list_tables mutate_
#' @importFrom tidyr separate_
#' @export
db_status <- function(path=NULL) {
  edb <- eidith_db(path)
  if(!(all(db_tables %in% db_list_tables(edb$con)))) {
    dbstatus<- list(status_msg = "Local EIDITH database is empty, out-of-date, or corrupt.  Run ed_db_download() to update")
  } else {
    records = tbl(edb, "sqlite_stat1") %>% collect() %>%
      filter_('tbl != "status"') %>%
      separate_("stat", into=c("rows", "columns"), sep=" ", convert=TRUE) %>%
      group_by_("tbl") %>%
      summarise_(rows=~max(rows)) %>%
      arrange_("rows") %>%
      mutate_(string = ~paste(prettyNum(rows, big.mark=","), tbl))
    dbstatus <-lst(countries = tbl(edb, "events") %>%
                     group_by_("country") %>% summarise_(n=~n()) %>% collect() %>% use_series("country") %>% sort(),
                   n_countries = length(countries),
                   last_modified_records = quicktime2(map_chr(db_tables[1:5], function(db_table) {
                     DBI::dbGetQuery(edb$con, paste0("SELECT MAX(date_modified) FROM ", db_table ))[[1]]
                   })),
                   last_modified_record = max(last_modified_records),
                   last_table = db_tables[1:5][last_modified_records == last_modified_record],
                   last_download = DBI::dbGetQuery(edb$con, "SELECT last_download FROM status")[[1]],
                   records = records,
                   status_msg = paste0(c(
                     paste(strwrap(paste0(c(
                       "Local EIDITH database holds data from ", n_countries, " countries: ",
                       paste(countries, collapse = "; ")
                       ), collapse=""), width=80, exdent=2), collapse="\n"), "\n",
                     paste(strwrap(paste0(c(
                       "Records: ", paste(records$string, collapse="; ")
                       ), collapse = ""), width=80, exdent=2), collapse="\n"), "\n",
                     "Last download: ", as.character(last_download), "\n",
                     "Last updated record: ", as.character(last_modified_record), " in ", last_table, " table"), collapse=""))
  }
  class(dbstatus) <- c("dbstatus", class(dbstatus))
  dbstatus
}

#'@export
print.dbstatus <- function(dbstatus) {
  cat(dbstatus$status_msg)
}


#' @export
export_db <- function(filename, ...) {  #Exports the database file to new location.  options(eidith_db) should let you change it.
   file.copy(from = eidith_db()$path, to = filename, ...)
}

#'@importFrom stringi stri_replace_first_fixed
#'@importFrom dplyr collect tbl
#'@export
check_db_updates <- function(path = NULL) {
  last_download <- stri_replace_first_fixed(
    collect(tbl(eidith_db(path), "status"))$last_download,
    " ", "T")
  auth <- ed_auth()
  check_at <- endpoints[endpoints !="TestIDSpecimenID"]
  new_rows <- lapply(check_at, function(endpoint) {
         newdat <- ed_get(endpoint = endpoint, verbose = FALSE, lmdate_from = last_download, postprocess = FALSE, auth = auth)
         nrow(newdat)
    })
  is_new_data <- as.logical(unlist(new_rows))
  names(is_new_data) <- check_at
  if(all(!is_new_data)) {
    message("No new data since ", last_download, ".")
  } else {
  message("New data at endpoints: [", paste0(check_at[is_new_data], collapse=","), "]. Use ed_db_download() to update.")
  }
  return(is_new_data)
}
