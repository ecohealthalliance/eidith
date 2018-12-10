#' @importFrom stringi stri_subset_fixed
#' @importFrom dplyr filter collect %>% coalesce mutate
ed_db_field_check <- function(tb, path, df2 = ed2_metadata()){
  ed_tb <- tbl(eidith_db(path), tb) %>% head %>% collect
  df <- ed_metadata()
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




#' Get the status of the locally stored EIDITH database
#'
#' @description
#' This function provides:
#'
#' @return A message regarding the status of current EIDITH database, or a prompt allowing the user
#'   to download missing / corrupt tables
#' @param path if provided, the filename of the sqlite database to check. By default,
#'   the function checks the status of the internal database or that with the global option `"ed_sql_path"`.
#' @param inter whether function is run interactively
#' @seealso  [ed_db_download()], [ed_db_updates()], [ed_db_export()]
#' @importFrom purrr map_chr keep
#' @importFrom dplyr tbl group_by_ summarise_ collect lst db_list_tables mutate_ lst_
#' @importFrom tidyr separate_
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom RSQLite dbGetQuery dbWriteTable
#' @export
ed_db_check_status <- function(path=NULL, inter = T) {
  status <- length(dbListTables(eidith_db(path))) > 0
  ed2_meta <- ed2_metadata()
  if(status == FALSE){
    if(interactive() & inter){
      dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database is missing.\nWould you like to download it?")

      if(dl_opt == 1) ed_db_download()
      if(dl_opt == 2) dbstatus <- list(status_msg1 ="There is no local EIDITH database.\nRun ed_db_download() to download.")
    }else{
      dbstatus <- list(status_msg1 ="There is no local EIDITH database.\nRun ed_db_download() to download.")
    }
  }else{
  edb <- eidith_db(path)
  dbstatus <- ""
  if(!(all(c(db_tables, db2_tables) %in% db_list_tables(edb)))) {
    #find out which tables are missing and then ask the user if they wish to download them?
    missing_p1_tables <- db_tables[which(db_tables %in% db_list_tables(edb) == FALSE)]
    missing_p2_tables <- db2_tables[which(db2_tables %in% db_list_tables(edb) == FALSE)]
    dl_p1_tables <- names(purrr::keep(p1_table_names, function(x) x %in% missing_p1_tables))
    dl_p2_tables <- names(purrr::keep(p2_table_names, function(x) x %in% missing_p2_tables))

    if(interactive() & inter){
    dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database is missing tables.\nWould you like to download missing tables?")

    if(dl_opt == 1) ed_db_download(dl_p1_tables, dl_p2_tables)
    if(dl_opt == 2) dbstatus <- list(status_msg1 ="Local EIDITH database is available, but missing tables.\nRun ed_db_check_status() to update")
    }else{
      dbstatus <- list(status_msg1 ="Local EIDITH database is available, but missing tables.\nRun ed_db_check_status() to update")
    }
  }else if(!all(sapply(c(db_tables[-7], db2_tables), function(x) ed_db_field_check(x, NULL, ed2_meta)))){
    #find out which tables have errors
    error_p1_tables <- sapply(db_tables[-7], function(x) ed_db_field_check(x, NULL, ed2_meta))
    error_p2_tables <- sapply(db2_tables, function(x) ed_db_field_check(x, NULL, ed2_meta))
    dl_p1_tables <- names(purrr::keep(p1_table_names, function(x) x %in% error_p1_tables))
    dl_p2_tables <- names(purrr::keep(p2_table_names, function(x) x %in% error_p2_tables))

    if(interactive() & inter){
    dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database has tables with corrupt or empty fields.\nWould you like to re-download these tables to correct errors?")

    if(dl_opt == 1) ed_db_download(dl_p1_tables, dl_p2_tables)
    if(dl_opt == 2) dbstatus <- list(status_msg2 ="Local EIDITH database fields are empty or corrupt. Use ed_db_download() to attempt a clean install.")
    }else{
      dbstatus <- list(status_msg ="Local EIDITH database fields are empty or corrupt. Use ed_db_download() to attempt a clean install.")
    }
  }else{
    dbstatus <- "Local EIDITH database contains all tables with all expected fields!\n"
  }
  }
  if(interactive() & inter){
   cat_line(green(dbstatus))
  }else{
  return(green(dbstatus))
  }

}


ed_db_presence <- function(){
  status <- length(dbListTables(eidith_db())) > 0
  if(status == FALSE){
    line1 <- (cli::rule(crayon::bold("Welcome to the EIDITH R Package!")))
    return(line1)
    }else{
    ed_create_banner()
  }
}


#' @importFrom DBI dbReadTable
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr %>% group_by summarize filter mutate
#' @importFrom purrr keep map
#' @importFrom cli rule symbol
#' @importFrom crayon cyan black green red magenta
#' @importFrom stringr str_detect
ed_create_banner <- function(path = NULL){
  edb <- eidith_db(path)
  tryCatch(expr = {
    download_dates <- dbReadTable(edb, "status") %>%
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
      p1_status_list <- purrr::map(p1_api_endpoints(), function(x){
        ind <- which(predict_1$display_name == x)
        if(length(ind) == 0){
          return(glue(crayon::red(cli::symbol$cross), "  ", x))
        }else{
          return(glue(crayon::green(cli::symbol$tick), "  ", crayon::cyan(x), crayon::black(" Table"),
                      glue_collapse(rep(" ", max(nchar(p1_api_endpoints())) + 5 - nchar(x))),
                      crayon::magenta(glue("Last Downloaded: ",
                                           as.character(predict_1$most_recent[ind])))))
        }
      })
    })

    suppressWarnings({
      p2_status_list <- purrr::map(p2_api_endpoints(), function(x){
        ind <- which(predict_2$display_name == x)
        if(length(ind) == 0){
          return(glue(crayon::red(cli::symbol$cross), "  ", crayon::red(x), crayon::black(" Table")))
        }else{
          return(glue(crayon::green(cli::symbol$tick), "  ", crayon::cyan(x), crayon::black(" Table"),
                      glue_collapse(rep(" ", max(nchar(p2_api_endpoints())) + 5 - nchar(x))),
                      crayon::magenta(glue("Last Downloaded: ",
                                           as.character(predict_2$most_recent[ind])))))
        }
      })
    })

    ed_banner <- glue(cli::rule(crayon::black(crayon::bold("EIDITH R Package"))),
                      crayon::black(crayon::italic("PREDICT-1 Table Status:")),
                      glue_collapse(p1_status_list, sep = "\n"),
                      crayon::black(crayon::italic("PREDICT-2 Table Status:")),
                      glue_collapse(p2_status_list, sep = "\n"), "\n","",
                      .sep = "\n")

    return(ed_banner)
  },
  error = function(err){
    error_line <- (green("There are unspecified errors in the local EIDITH database. If problems persist after using ed_db_download(), see ?ed_contact.\n"))
    return(paste(error_line, err, sep = "\n"))
    }

  )
}



#' @importFrom DBI dbListTables
ed_db_make_status_msg <- function(path = NULL){
  edb <- eidith_db(path)
  if("status" %in% dbListTables(edb)){
  dbExecute(edb, "analyze")
  download_dates <- dbReadTable(edb, "status") %>%
    group_by(t_name) %>%
    summarize(most_recent = max(as.Date(last_download)))
  records <- dbReadTable(edb, "sqlite_stat1")
  p1_records <- filter(records, str_detect(tbl, "2") == FALSE & tbl != "status")
  p2_records <- filter(records, str_detect(tbl, "2"))
  #mutate_(string = ~paste(prettyNum(rows, big.mark=","), tbl))
  tables <- dbListTables(edb)

  if("events" %in% tables){
    p1_countries <- dbReadTable(edb, "events") %>%
      group_by_("country") %>%
      summarise_(n=~n()) %>%
      collect() %>%
      `$`("country") %>%
      sort()
  } else{
    p1_countries <- NULL
  }
  if("events_2" %in% tables){
    p2_countries <- dbReadTable(edb, "events_2") %>%
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
    n_countries = n_countries
    # last_modified_records = ~quicktime2(map_chr(db_tables[1:5], function(db_table) {
    #   DBI::dbGetQuery(edb, paste0("SELECT MAX(date_modified_",db_table,") FROM ", db_table ))[[1]]
    # })),
    # last_modified_record = ~max(last_modified_records),
    # last_table = ~db_tables[1:5][last_modified_records == last_modified_record],
    # last_download = ~DBI::dbGetQuery(edb, "SELECT last_download FROM status")[[1]],
    # records = records
  )
  } else{
    dbstatus <- ""
  }
  class(dbstatus) <- c("dbstatus", class(dbstatus))
  dbstatus
}


ed_db_status_msg <- function(status) {
  if(length(status) == 1) return("")
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
print.dbstatus <- function(x,...) {
  cat(ed_db_status_msg(ed_db_make_status_msg()))
}


#' Obtain and print status of all local EIDITH tables
#'
#' @description
#' This function evaluates and prints a detailed banner of local EIDITH PREDICT-1
#' and PREDICT-2 table statuses. This includes whether they are currently present,
#' their last download date, and the PREDICT-1 and PREDICT-2 countries that
#' are available in the local database.
#'

#'@export
ed_db_detailed_status <- function() {
  cat(ed_db_presence())
  cat(ed_db_status_msg(ed_db_make_status_msg()))
}




