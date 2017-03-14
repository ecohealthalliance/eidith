db_tables <- c("events", "animals", "specimens", "tests", "viruses",
               "test_specimen_ids", "status")

db_unique_indexes <- list(
  events = list("event_id"),
  animals = list("animal_id"),
  specimens = list("specimen_id"),
  tests = list("test_id"),
  viruses = list("sequence_id"),
  test_specimen_ids = list()
)

db_other_indexes <- list(
  events = list("country"),
  animals = list("event_id"),
  specimens = list("animal_id", "specimen_id_name"),
  tests = list("specimen_id_names"),
  viruses = list("test_id"),
  test_specimen_ids = list("test_id", "specimen_id")
)

#' @importFrom stringi stri_subset_fixed
ed_db_field_check <- function(tb){
    ed_tb <- tbl(eidith_db(), tb) %>% head %>% collect
    expected_fields <- filter(ed_metadata(), table == tb) %>%
      mutate(nname = coalesce(replacement_name, auto_processed_name)) %>%
      `$`(nname) %>%
      na.omit() %>%
      stri_subset_fixed("DROP", negate=TRUE)
    condition_a <- (all(names(ed_tb %in% expected_fields)))
    condition_b <- (all(names(expected_fields %in% ed_tb)))
    return(condition_a & condition_b)
}

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
#' Each time it is run it downloads into a temporary database and checks for the presence of
#' all tables before it clears out the previous data, preventing the local database from becoming damaged if
#' there are problems with the download. Each function run will download the entire database, and
#' as such it takes a few minutes.  In the future, we will allow for updating only with new and changed records.
#'
#' The function will prompt for username and password unless you have [cached your credentials][ed_auth].
#'
#' @importFrom dplyr db_list_tables db_drop_table copy_to
#' @importFrom RSQLite sqliteCopyDatabase
#' @param verbose Show messages while in progress?
#' @seealso [ed_db_status()], [ed_db_updates()], [ed_db_export()]
#' @export
ed_db_download <- function(verbose=interactive()) {
  auth <- ed_auth(verbose = verbose)
  if(verbose) message("Downloading and processing EIDITH data. This may take a few minutes.")
  lapply(dplyr::db_list_tables(eidith_db(temp_sql_path())$con), function(x) {
    dplyr::db_drop_table(eidith_db(temp_sql_path())$con, x)}
  )
  lapply(seq_along(endpoints), function(x) {
    tb <- ed_get(endpoints[x], postprocess=TRUE, verbose=verbose, auth=auth)
    dplyr::copy_to(eidith_db(temp_sql_path()), tb, name=db_tables[x], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
    rm(tb);
    gc(verbose=FALSE)
  })
  dplyr::copy_to(eidith_db(temp_sql_path()), data.frame(last_download=as.character(Sys.time())),
                 name="status", temporary=FALSE)
  if(!(all(db_tables %in% db_list_tables(eidith_db(temp_sql_path())$con)))){
    message("NOTE: Newly downloaded EIDITH database is empty or corrupt, using previous version.")
    if(verbose) {
      message("Old Database Status:")
      message(ed_db_status_msg(ed_db_status()))
    }
    file.remove(temp_sql_path())
    return(invisible(0))
  }else if(!all(sapply(db_list_tables(eidith_db(temp_sql_path())$con), function(x) ed_db_field_check(x)))){
    message("NOTE: Newly downloaded EIDITH database lacks the correct fields, using previous version.")
    if(verbose) {
      message("Old Database Status:")
      message(ed_db_status_msg(ed_db_status()))
    }
    file.remove(temp_sql_path())
    return(invisible(0))
  }else{
    if(verbose) {
      message("Database successfully downloaded!")
    }
    lapply(dplyr::db_list_tables(eidith_db()$con), function(x) {
      dplyr::db_drop_table(eidith_db()$con, x)}
    )
    RSQLite::sqliteCopyDatabase(eidith_db(temp_sql_path())$con, eidith_db()$con)
    file.remove(temp_sql_path())
    message(ed_db_status_msg(ed_db_status()))
  return(invisible(0))
  }
}



#' Get the status of the locally stored EIDITH database
#'
#' @description
#' This function provides:
#'  - Countries in the database
#'  - Size of each table in the database
#'  - The most recent download date
#'  - The last-updated records in local database
#'
#' @return A list of database status information, pretty-printed.
#' @param path if provided, the filename of the sqlite database to check. By default,
#'   the function checks the status of the internal database or that with the global option `"ed_sql_path"`.
#' @seealso  [ed_db_download()], [ed_db_updates()], [ed_db_export()]
#' @importFrom purrr map_chr
#' @importFrom dplyr tbl group_by_ summarise_ collect lst db_list_tables mutate_ lst_
#' @importFrom tidyr separate_
#' @importFrom DBI dbGetQuery
#' @importFrom RSQLite dbGetQuery
#' @export
ed_db_status <- function(path=NULL) {
  edb <- eidith_db(path)
  if(!(all(db_tables %in% db_list_tables(edb$con)))) {
    dbstatus <- list(status_msg ="Local EIDITH database tables are empty, out-of-date, or corrupt.\nRun ed_db_download() to update")
  } else if(!all(sapply(db_list_tables(edb$con), function(x) ed_db_field_check(x)))){
    dbstatus <- list(status_msg ="Local EIDITH database fields are empty, out-of-date, or corrupt.\nRun ed_db_download() to update")
  } else {
    records <- tbl(edb, "sqlite_stat1") %>% collect() %>%
      filter_('tbl != "status"') %>%
      separate_("stat", into=c("rows", "columns"), sep=" ", convert=TRUE) %>%
      group_by_("tbl") %>%
      summarise_(rows=~max(rows)) %>%
      arrange_("rows") %>%
      mutate_(string = ~paste(prettyNum(rows, big.mark=","), tbl))
    dbstatus <-lst_(list(
      countries = ~tbl(edb, "events") %>%
        group_by_("country") %>%
        summarise_(n=~n()) %>%
        collect() %>%
        `$`("country") %>%
        sort(),
      n_countries = ~length(countries),
      last_modified_records = ~quicktime2(map_chr(db_tables[1:5], function(db_table) {
        DBI::dbGetQuery(edb$con, paste0("SELECT MAX(date_modified_",db_table,") FROM ", db_table ))[[1]]
      })),
      last_modified_record = ~max(last_modified_records),
      last_table = ~db_tables[1:5][last_modified_records == last_modified_record],
      last_download = ~DBI::dbGetQuery(edb$con, "SELECT last_download FROM status")[[1]],
      records = ~records
    ))
  }
  class(dbstatus) <- c("dbstatus", class(dbstatus))
  dbstatus
}

ed_db_status_msg <- function(status) {
  if(is.null(status[["n_countries"]])) {
    status_msg <- status[["status_msg"]]
  } else {
    status_msg <- paste0(c(
      paste(strwrap(paste0(c(
        "Local EIDITH database holds data from ", status[["n_countries"]], " countries: ",
        paste(status[["countries"]], collapse = "; ")
      ), collapse=""), width=80, exdent=2), collapse="\n"), "\n",
      paste(strwrap(paste0(c(
        "Records: ", paste(status[["records"]][["string"]], collapse="; ")
      ), collapse = ""), width=80, exdent=2), collapse="\n"), "\n",
      "Last download: ", as.character(status[["last_download"]]), "\n",
      "Last updated record: ", as.character(status[["last_modified_record"]]),
        " in ", status[["last_table"]], " table"), collapse="")
  }
  return(status_msg)
}

#'@export
print.dbstatus <- function(x, ...) {
  cat(ed_db_status_msg(x))
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
ed_db_export <- function(filename, ...) {  #Exports the database file to new location.  options(eidith_db) should let you change it.
  file.copy(from = eidith_db()$path, to = filename, ...)
}

#' Check the online EIDITH database for updates since your last download.
#'
#' This function checks to see if the EIDITH online database has been updated
#' since the last time [ed_db_download()] was run. If it has, you may run
#' [ed_db_download()] to get the latest data.
#'
#' @param path if provided, the filename of the sqlite database to check. By default,
#'   the function checks the status of the internal database or that with the global option `"ed_sql_path"`.
#' @return A named vector, showing TRUE for tables that have been updated.  A message is also printed.
#' @seealso  [ed_db_status()], [ed_db_download()],  [ed_db_export()]
#'@importFrom stringi stri_replace_first_fixed
#'@importFrom dplyr collect tbl
#'@export
ed_db_updates <- function(path = NULL) {
  last_download <- stri_replace_first_fixed(
    collect(tbl(eidith_db(path), "status"))$last_download,
    " ", "T")
  auth <- ed_auth()
  check_at <- endpoints[endpoints !="TestIDSpecimenID"]
  new_rows <- lapply(check_at, function(endpoint) {
    newdat <- ed_get(endpoint = endpoint, verbose = FALSE,
                     lmdate_from = last_download, postprocess = FALSE, auth = auth)
    nrow(newdat)
  })
  is_new_data <- as.logical(unlist(new_rows))
  names(is_new_data) <- check_at
  if(all(!is_new_data)) {
    message("No new data since ", last_download, ".")
  } else {
    message("New data at endpoints: [",
            paste0(check_at[is_new_data], collapse=","),
            "]. Use ed_db_download() to update.")
  }
  return(is_new_data)
}
