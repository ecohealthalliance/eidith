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
#' @importFrom dplyr tbl group_by_ summarise_ collect lst mutate_ lst_
#' @importFrom tidyr separate_
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @importFrom RSQLite dbGetQuery dbWriteTable
#' @export
ed_db_check_status <- function(path=NULL, inter = T) {

  edb <- eidith_db(path)
  edt <- dbListTables(edb)
  ed2_meta <- ed2_metadata()

  # check that tables exist
  tables_exist <- length(edt) > 0

  # case when no tables exist
  if(!tables_exist){
    if(interactive() & inter){
      dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database is missing.\nWould you like to download it?")

      if(dl_opt == 1) ed_db_download()
      if(dl_opt == 2) dbstatus <- list(status_msg1 ="There is no local EIDITH database.\ned_db_download() to download.")

    }else{
      dbstatus <- list(status_msg1 ="There is no local EIDITH database.\ned_db_download() to download.")
    }
  }

  # check that status table is not corrupt
  status_table_not_corrupt <- ed_db_status_table()

  # case when status table corrupt
  if(!status_table_not_corrupt){
    dbstatus <- list(status_msg1 ="Database is out of date or corrupt. Delete and re-download:\ned_db_delete() followed by ed_db_download()")
  }

  # case where tables exist
  if(tables_exist & status_table_not_corrupt){

    dbstatus <- ""

    # find out if any tables are missing and then ask the user if they wish to download them?
    if(!(all(c(db2_tables) %in% dbListTables(edb)))) {
      missing_p2_tables <- db2_tables[which(db2_tables %in% dbListTables(edb) == FALSE)]
      dl_p2_tables <- names(purrr::keep(p2_table_names, function(x) x %in% missing_p2_tables))

      if(interactive() & inter){
        dl_opt <- menu(c("Yes", "No"), title = "Local EIDITH database is missing tables.\nWould you like to download missing tables?")

        if(dl_opt == 1) ed_db_download(dl_p2_tables)
        if(dl_opt == 2) dbstatus <- list(status_msg1 ="Local EIDITH database is available, but missing tables.\ned_db_check_status() to update")
      }else{
        dbstatus <- list(status_msg1 ="Local EIDITH database is available, but missing tables.\ned_db_check_status() to update")
      }
    }else{
      dbstatus <- "Local EIDITH database contains all tables with all expected fields!\n"
    }

    #check tables that exist for errors
    tbls_to_check <- edt[!grepl("sqlite|status", edt)]

    if(!all(sapply(tbls_to_check, function(x) eidith:::ed_db_field_check(x, NULL, ed2_meta)))){
      dbstatus <- list(status_msg ="Local EIDITH database fields are empty or corrupt. Delete and re-download:\ned_db_delete() followed by ed_db_download().")
    }
  }

  if(interactive() & inter){
    cat_line(cyan(dbstatus))
  }else{
    return(cyan(dbstatus))
  }
}


ed_db_presence <- function(){
  status <- all(length(dbListTables(eidith_db())) > 0 )#, ed_db_status_table()) # tables exist and status not corrupt
  if(!status){
    line1 <- (cli::rule(crayon::green("Welcome to the EIDITH R Package!")))
    line2 <- crayon::green(".  No database detected")
    return(glue::glue(line1, line2, .sep = "\n"))
  }else{
    ed_create_banner()
  }
}


#' @importFrom DBI dbReadTable dbListTables dbExecute
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr %>% group_by summarize filter mutate
#' @importFrom purrr keep map
#' @importFrom cli rule symbol
#' @importFrom crayon cyan green red magenta
#' @importFrom stringr str_detect
ed_create_banner <- function(path = NULL){

 # if(ed_db_status_table()){ #only run banner if status table is not corrupt

    edb <- eidith_db(path)
    tryCatch(expr = {
      download_dates <- dbReadTable(edb, "status") %>%
        group_by(t_name) %>%
        summarize(most_recent = max(as.Date(last_download)))

      predict_2 <- download_dates %>%
        filter(str_detect(t_name, "2") == TRUE)

      if(nrow(predict_2) > 0){
        predict_2 <- mutate(predict_2, display_name = unlist(purrr::map(t_name, function(x) unlist(names(purrr::keep(p2_table_names, function(y) y == x))))))
      }

      suppressWarnings({
        p2_status_list <- purrr::map(p2_api_endpoints(), function(x){
          ind <- which(predict_2$display_name == x)
          if(length(ind) == 0){
            return(glue(crayon::red(cli::symbol$cross), "  ", crayon::red(x)))
          }else{
            return(glue(crayon::green(cli::symbol$tick), "  ", crayon::green(x),
                        glue_collapse(rep(" ", max(nchar(p2_api_endpoints())) + 5 - nchar(x))),
                        crayon::magenta(glue("Last Downloaded: ",
                                             as.character(predict_2$most_recent[ind])))))
          }
        })
      })

      ed_banner <- glue(cli::rule(crayon::cyan(crayon::bold("EIDITH R Package"))),
                        crayon::cyan(crayon::italic("\nPREDICT Table Status:")),
                        glue_collapse(p2_status_list, sep = "\n"), "\n","",
                        .sep = "\n")

      return(ed_banner)
    },
    error = function(err){
      error_line <- (red("There are unspecified errors in the local EIDITH database. If problems persist after using ed_db_download(), see ?ed_contact.\n"))
      eidith_disconnect(.eidith_env)
      return(paste(error_line, err, sep = "\n"))
    }

    )
  #}
}

#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export
ed_db_status_table <- function(path=NULL) {

  edb <- eidith_db(path)
  edt <- dbListTables(edb)

  status_exists <- "status" %in% edt

  status_table_not_corrupt <- if(status_exists){
    dbExecute(edb, "analyze")
    status_names <- dbReadTable(edb, "status") %>% colnames()
    all(length(status_names) == 3, status_names %in% c("unique_id", "t_name", "last_download"))
  }else{TRUE}

  return(status_table_not_corrupt)
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
}




