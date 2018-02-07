db_tables <- c("events", "animals", "specimens", "tests", "viruses",
               "test_specimen_ids", "status")

db2_tables <- c("events_2", "animals_2", "specimens_2", "animal_production_2")

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
  AnimalProduction = "animal_production_2"
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
  animal_production_2 = list("integer_id")
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
  animal_production_2 = list("event_name")
)

#' @importFrom stringi stri_subset_fixed
#' @importFrom dplyr filter collect %>% coalesce mutate
ed_db_field_check <- function(tb, path){
    ed_tb <- tbl(eidith_db(path), tb) %>% head %>% collect
    df <- ed_metadata()
    df2 <- ed2_metadata()
    expected_fields <- filter(df, df$table == tb) %>%
      mutate(nname = coalesce(replacement_name, auto_processed_name)) %>%
      `$`(nname) %>%
      na.omit() %>%
      stri_subset_fixed("DROP", negate=TRUE)
    expected_fields_2 <- filter(df2, df2$table == tb) %>%
      mutate(nname = coalesce(replacement_name, auto_processed_name)) %>%
      `$`(nname) %>%
      na.omit() %>%
      stri_subset_fixed("DROP", negate=TRUE)
    table_names <- names(ed_tb)[names(ed_tb) != "integer_id"]
    condition_a <- (all(table_names %in% (expected_fields)))
    condition_b <- (all(expected_fields %in% table_names))
    condition_c <- (all(table_names %in% (expected_fields_2)))
    condition_d <- (all(expected_fields_2 %in% table_names))
    return((condition_a & condition_b) | (condition_c & condition_d))
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
#' @importFrom dplyr db_list_tables db_drop_table group_by %>% pull
#' @importFrom RSQLite sqliteCopyDatabase
#' @importFrom DBI dbWriteTable
#' @param verbose Show messages while in progress?
#' @seealso [ed_db_status()], [ed_db_updates()], [ed_db_export()]
#' @export
ed_db_download <- function(p1_tables = endpoints, p2_tables = endpoints2, verbose=interactive()) {
  auth <- ed_auth(verbose = verbose)
  if(verbose) message("Downloading and processing EIDITH data. This may take a few minutes.")
  lapply(dplyr::db_list_tables(eidith_db(temp_sql_path())$con), function(x) {
    dplyr::db_drop_table(eidith_db(temp_sql_path())$con, x)}
  )
  #P1 tables
  lapply(p1_tables, function(x) {
    tb <- ed_get(x, postprocess=TRUE, verbose=verbose, auth=auth)
    dplyr::copy_to(eidith_db(temp_sql_path()), tb, name=p1_table_names[[x]], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
    rm(tb);
    gc(verbose=FALSE)
  })
  # P2
  p2_key_errors <- data_frame(table = character(), field_name = character(), multiple_id = character())
  lapply(p2_tables, function(x) {
    tb <- ed2_get(x, postprocess=TRUE, verbose=verbose, auth=auth)
    lc_name <- p2_table_names[[x]]

    intended_key <- db_other_indexes[[lc_name]][[1]]
    group_var <- as.name(intended_key)

    key_check <- tb %>%
      group_by(!!group_var) %>%
      summarize(count = n()) %>%
      filter(count > 1) %>%
      dplyr::pull(!!group_var)
    errors <- data_frame(table = x, field_name = intended_key, multiple_id = key_check)
    p2_key_errors <<- rbind(p2_key_errors, errors)
    tb$integer_id <- seq_len(nrow(tb))
    dplyr::copy_to(eidith_db(temp_sql_path()), tb, name=p2_table_names[[x]], temporary = FALSE,
                   unique_indexes = db_unique_indexes[[x]], indexes = db_other_indexes[[x]])
    rm(tb);
    gc(verbose=FALSE)
  })

  if(nrow(p2_key_errors) > 0){
    dbWriteTable(eidith_db(temp_sql_path())$con, p2_key_errors, name = "p2_unique_id_errors", append = TRUE)
  }
  #Check that requested tables have downloaded:
  p1_dls <- unname(sapply(p1_tables, function(x) p1_table_names[[x]]))
  p2_dls <- unname(sapply(p2_tables, function(x) p2_table_names[[x]]))
  if(!(all(c(p1_dls, p2_dls) %in% db_list_tables(eidith_db(temp_sql_path())$con)))){
    message("NOTE: Newly downloaded EIDITH database is empty or corrupt, using previous version.")
    if(verbose) {
      message("Old Database Status:")
      message(ed_db_status_msg(ed_db_check_status()))
    }
    return(invisible(0))
  }else if(!all(sapply(c(p1_dls, p2_dls), function(x) ed_db_field_check(x,temp_sql_path())))){
    message("NOTE: Newly downloaded EIDITH database lacks the correct fields, using previous version.")
    if(verbose) {
      message("Old Database Status:")
      message(ed_db_status_msg(ed_db_check_status()))
    }
    return(invisible(0))
  }else{
    if(verbose) {
      message("Database successfully downloaded!")
    }

    lapply(c(p1_dls, p2_dls), function(x){
      temp_tbl <- RSQLite::dbReadTable(eidith_db(temp_sql_path())$con, x)
      dbWriteTable(eidith_db()$con, value = temp_tbl, name = x, overwrite = TRUE)
    })

    # creating status
    status_df <- data.frame(unique_id = seq_along(c(p1_dls, p2_dls)), t_name = unlist(c(p1_dls, p2_dls)), last_download = as.character(Sys.time()))

    if("status" %in% db_list_tables(eidith_db()$con)){
      dbWriteTable(eidith_db()$con, name = "status", value = status_df, append = TRUE, row.names = FALSE)
    }else{
      dbWriteTable(eidith_db()$con, value = status_df,
                   name="status", row.names = FALSE)
    }
    file.remove(temp_sql_path())
    message(ed_create_banner(), ed_db_status_msg(ed_db_make_details()), ed_db_check_status())
  return(invisible(0))
  }
}



#' Get the status of the locally stored EIDITH database
#'
#' @description
#' This function provides:
#'
#' @return A list of database status information, pretty-printed.
#' @param path if provided, the filename of the sqlite database to check. By default,
#'   the function checks the status of the internal database or that with the global option `"ed_sql_path"`.
#' @seealso  [ed_db_download()], [ed_db_updates()], [ed_db_export()]
#' @importFrom purrr map_chr keep
#' @importFrom dplyr tbl group_by_ summarise_ collect lst db_list_tables mutate_ lst_
#' @importFrom tidyr separate_
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom RSQLite dbGetQuery dbWriteTable
#' @export
ed_db_check_status <- function(path=NULL) {
  edb <- eidith_db(path)
  dbstatus <- ""
  if(!(all(c(db_tables, db2_tables) %in% db_list_tables(edb$con)))) {
    #find out which tables are missing and then ask the user if they wish to download them?
    missing_p1_tables <- db_tables[which(db_tables %in% db_list_tables(edb$con) == FALSE)]
    missing_p2_tables <- db2_tables[which(db2_tables %in% db_list_tables(edb$con) == FALSE)]
    dl_p1_tables <- names(purrr::keep(p1_table_names, function(x) x %in% missing_p1_tables))
    dl_p2_tables <- names(purrr::keep(p2_table_names, function(x) x %in% missing_p2_tables))

    dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database is missing tables.\nWould you like to download missing tables?")

    if(dl_opt == 1) ed_db_download(dl_p1_tables, dl_p2_tables)
    if(dl_opt == 2) dbstatus <- list(status_msg ="Local EIDITH database is available, but missing tables.\nRun ed_db_download() to update")

  }else if(!all(sapply(c(db_tables[-7], db2_tables), function(x) ed_db_field_check(x, NULL)))){
    #find out which tables have errors
    error_p1_tables <- sapply(db_tables[-7], function(x) ed_db_field_check(x, NULL))
    error_p2_tables <- sapply(db2_tables, function(x) ed_db_field_check(x, NULL))
    dl_p1_tables <- names(purrr::keep(p1_table_names, function(x) x %in% error_p1_tables))
    dl_p2_tables <- names(purrr::keep(p2_table_names, function(x) x %in% error_p2_tables))
    dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database has tables with corrupt or empty fields.\nWould you like to re-download these tables to correct errors?")

    if(dl_opt == 1) ed_db_download(dl_p1_tables, dl_p2_tables)
    if(dl_opt == 2) dbstatus <- list(status_msg ="Local EIDITH database fields are empty or corrupt.")
  }
  class(dbstatus) <- c("dbstatus", class(dbstatus))
  dbstatus
}


#' @importFrom DBI dbReadTable
#' @importFrom glue glue collapse
#' @importFrom dplyr %>% group_by summarize filter mutate
#' @importFrom purrr keep map
#' @importFrom cli rule symbol
#' @importFrom crayon cyan black green red magenta
#' @importFrom stringr str_detect
ed_create_banner <- function(){
  edb <- eidith_db()

  tryCatch(expr = {
  download_dates <- dbReadTable(edb$con, "status") %>%
    group_by(t_name) %>%
    summarize(most_recent = max(as.Date(last_download)))

  predict_1 <- download_dates %>%
    filter(str_detect(t_name, "2") == FALSE)

  if(nrow(predict_1) > 0){
    predict_1 <- predict_1  %>%
      mutate(display_name = unlist(purrr::map(t_name, function(x) unlist(names(purrr::keep(p1_table_names, function(y) y == x))))))
  }

  predict_2 <- download_dates %>%
    filter(str_detect(t_name, "2") == TRUE)

  if(nrow(predict_2) > 0){
    predict_2 <- mutate(predict_2, display_name = unlist(purrr::map(t_name, function(x) unlist(names(purrr::keep(p2_table_names, function(y) y == x))))))
  }


suppressWarnings({
  p1_status_list <- purrr::map(endpoints, function(x){
    ind <- which(predict_1$display_name == x)
    if(ind == 0){
      return(glue(crayon::red(cli::symbol$cross), "  ", x))
    }else{
      return(glue(crayon::green(cli::symbol$tick), "  ", crayon::cyan(x), crayon::black(" Table"),
                  collapse(rep(" ", max(nchar(endpoints)) + 5 - nchar(x))),
                  crayon::magenta(glue("Last Downloaded: ",
                                       as.character(predict_1$most_recent[ind])))))
    }
  })
})

suppressWarnings({
  p2_status_list <- purrr::map(endpoints2, function(x){
    ind <- which(predict_2$display_name == x)
    if(length(ind) == 0){
      return(glue(crayon::red(cli::symbol$cross), "  ", crayon::red(x), crayon::black(" Table")))
    }else{
      return(glue(crayon::green(cli::symbol$tick), "  ", crayon::cyan(x), crayon::black(" Table"),
                  collapse(rep(" ", max(nchar(endpoints2)) + 5 - nchar(x))),
                  crayon::magenta(glue("Last Downloaded: ",
                                       as.character(predict_2$most_recent[ind])))))
    }
  })
})

    ed_banner <- glue(cli::rule(crayon::black(crayon::bold("EIDITH R Package"))),
                      crayon::black(crayon::italic("PREDICT-1 Table Status:")),
                      collapse(p1_status_list, sep = "\n"),
                      crayon::black(crayon::italic("PREDICT-2 Table Status:")),
                      collapse(p2_status_list, sep = "\n"), "\n",
                      .sep = "\n")

    return(ed_banner)
  },
  error = function(err){
    cat(cli::rule(crayon::bold("Welcome to the EIDITH R Package!")))
    cat("\n")
    message("There is no local EIDITH database, please follow the prompts or use ed_db_download() to download EIDITH data.")

    p_opt <- menu(c("Yes", "No"), title = "Would you like to download EIDITH database?")
    if(p_opt == 1){
      ed_db_download(p2_tables = c("Event", "Animal", "Specimen"))
    }
  }
  )
}


#' @importFrom DBI dbListTables
ed_db_make_details <- function(path = NULL){
  edb <- eidith_db(path)
  dbExecute(edb$con, "analyze")
  download_dates <- dbReadTable(edb$con, "status") %>%
    group_by(t_name) %>%
    summarize(most_recent = max(as.Date(last_download)))
  records <- dbReadTable(edb$con, "sqlite_stat1")
  p1_records <- filter(records, str_detect(tbl, "2") == FALSE & tbl != "status")
  p2_records <- filter(records, str_detect(tbl, "2"))
  #mutate_(string = ~paste(prettyNum(rows, big.mark=","), tbl))
  tables <- dbListTables(edb$con)

  if("events" %in% tables){
  p1_countries <- dbReadTable(edb$con, "events") %>%
    group_by_("country") %>%
    summarise_(n=~n()) %>%
    collect() %>%
    `$`("country") %>%
    sort()
  } else{
    p1_countries <- NULL
  }
  if("events_2" %in% tables){
  p2_countries <- dbReadTable(edb$con, "events_2") %>%
    group_by_("country") %>%
    summarise_(n=~n()) %>%
    collect() %>%
    `$`("country") %>%
    sort()
  } else{
    p2_countries <- NULL
  }

  n_countries <- length(unique(c(p2_countries, p1_countries)))
  if (is.null(p1_countries) | is.null(p2_countries)){
    n_countries <- n_countries - 1
  }

  dbstatus <-list(
    p1_countries = p1_countries,
    p2_countries = p2_countries,
    n_countries = n_countries,
    # last_modified_records = ~quicktime2(map_chr(db_tables[1:5], function(db_table) {
    #   DBI::dbGetQuery(edb$con, paste0("SELECT MAX(date_modified_",db_table,") FROM ", db_table ))[[1]]
    # })),
    # last_modified_record = ~max(last_modified_records),
    # last_table = ~db_tables[1:5][last_modified_records == last_modified_record],
    # last_download = ~DBI::dbGetQuery(edb$con, "SELECT last_download FROM status")[[1]],
    records = records
  )

class(dbstatus) <- c("dbstatus", class(dbstatus))
  dbstatus
}


ed_db_status_msg <- function(status) {
  if(is.null(status[["n_countries"]])) {
    status_msg <- status[["status_msg"]]
  } else {
    status_msg <-
      paste(strwrap(paste0(c(
        crayon::blue(paste0("Local EIDITH database holds data from ", status[["n_countries"]], " countries: ")), crayon::green("PREDICT-1 countries: "),
        crayon::black(paste(status[["p1_countries"]], collapse = "; ")), crayon::green("PREDICT-2 countries: "), crayon::black(paste(status[["p2_countries"]], collapse = "; "))
      )), width=80, exdent=2), "\n")








       # paste(strwrap(paste0(c(
      #   "Records: ", paste(status[["records"]][["string"]], collapse="; ")
      # ), collapse = ""), width=80, exdent=2), collapse="\n"), "\n",
   #   "Last download: ", "FILLER", "\n")
     # "Last updated record: ", as.character(status[["last_modified_record"]]),
     #   " in ", status[["last_table"]], " table"), collapse="")
  }
  return(status_msg)
}

#'@export
print.dbstatus <- function(x, ...) {
  cat(ed_db_status_msg(ed_db_make_details(x)))
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
  sqliteCopyDatabase(eidith_db()$con, filename, ...)
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
ed_db_updates <- function(path = NULL) {    # NEEDS TO BE RE-WORKED
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
