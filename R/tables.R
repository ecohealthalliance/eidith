logical_vars <- c("archived_data", "deep_forest_data", "prioritized_for_testing",
                  "pooled", "message_sent_to_country", "message_sent_to_govt",
                  "govt_approved_release", "predict_protocol", "known",
                  "human_health")

date_vars <- c("event_date", "sample_date", "specimen_date",
               "test_date", "lab_submission_date", "results_date")

datetime_vars <- c("date_created", "date_modified", "database_date")

#' Load PREDICT-1 EIDITH data from the local database
#'
#' These functions load data from the locally-stored SQLite database of downloaded
#' and cleaned EIDITH data. `ed_table` is a general function, and `ed_table_`
#' it's [standard evaluation](https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html)
#' equivalent, useful for programming.  The other functions are convenience aliases
#' for the individual tables.
#' Alternate versions return [mock data] `ed_mock()`` for
#' tutorials and practice.
#'
#' These functions take [dplyr::filter()] arguments to sub-set the data.  Using
#' these, the data is subsetted via SQL *before* it is loaded into memory.
#' For large tables, such as the *tests* table, this is useful for reducing the memory footprint of your R session.
#'
#' Note that subsetting in SQL is more limited:
#'
#' -  Use `0` or `1` instead of `TRUE` or `FALSE`
#' -  Dates are stored as character strings, but as they are in YYYY-MM-DD
#'    format, filtering such as `event_date > "2014-01-01"` still works.
#'
#' @param table one of the EIDITH PREDICT-1 or PREDICT-2 database tables.
#' @param ... arguments passed to [dplyr::filter()] to subset data
#' @param .dots standard-evaluation versions of subsetting arguments
#' @return a [tibble][tibble::tibble]-style data frame.
#' @importFrom dbplyr partial_eval
#' @importFrom dplyr tbl tbl_df filter_ mutate_at funs_ funs collect vars n
#' @importFrom stringi stri_replace_first_regex stri_extract_last_regex stri_detect_fixed
#' @export
#' @rdname ed_table
ed_table_ <- function(table, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  dots = lazyeval::as.lazy_dots(   #This stuff deals with the dplyr bug found at https://github.com/hadley/dplyr/issues/511 by modifying "%in%" calls
    lapply(dots, function(dot_expr) {
      new_expr <- paste0(
        deparse(partial_eval(dot_expr[["expr"]], env=dot_expr[["env"]]),
                width.cutoff = 500L),
        collapse = "")
      if(stri_detect_fixed(new_expr, "%in%")) {
        matched_expr <- stri_extract_last_regex(new_expr, "(?<=%in%\\s).*$")
        if(length(eval(parse(text=matched_expr))) ==  0 ) {
          new_expr <- stri_replace_first_fixed(new_expr, matched_expr, "('')")
        }
        else if(length(eval(parse(text=matched_expr))) == 1) {
          new_expr <- stri_replace_first_fixed(new_expr, matched_expr,
                                               paste0("(", matched_expr, ")"))
        }
      }
      lazyeval::as.lazy(new_expr, env=dot_expr[["env"]])
    }))


  #add class attributes to P2 tables
  # if(stri_detect_fixed(table, "2")){
  #   ed_tb <- dbReadTable(eidith_db(), table)
  #   #adding notes
  #   ed_tb <- ed_tb %>%
  #     filter_(.dots=dots)
  #   note_cols <- which(stri_detect_fixed(names(ed_tb), "notes"))
  #   if(length(note_cols > 0)){
  #     filled_notes <- sapply(note_cols, function(x) all(!is.na(ed_tb[,x])))
  #     msg_notes <- note_cols[filled_notes]
  #     if(length(msg_notes > 0)) attr(ed_tb, "notes") <- msg_notes
  #   }
  #   #adding duplicate rows
  #   intended_key <- db_other_indexes[[table]][[1]]
  #   group_var <- as.name(intended_key)
  #   key_errors <- ed_tb %>%
  #     group_by(!!group_var) %>%
  #     summarize(count = n()) %>%
  #     filter(count > 1) %>%
  #     dplyr::pull(!!group_var)
  #   attr(ed_tb, "duplicate_keys") <- key_errors
  #   indices <- which(ed_tb[[intended_key]] %in% key_errors)
  #   attr(ed_tb, "duplicate_indices") <- indices
  #   class(ed_tb) <- c("eidith_tbl", class(ed_tb))
  #
  #   #print details on local database pull"
  #   if(interactive()){
  #   print(ed_tb, tibble_print = FALSE)
  # }
  # }else{
    ed_tb <- dbReadTable(eidith_db(), table)
    ed_tb <- ed_tb %>%
      filter_(.dots=dots) %>%
      fix_classes() %>%
      as.tibble()
 # }
ed_tb
}




# Add mention of attributes on pull call

#'@importFrom stringr str_detect
#'@importFrom cli cat_line
#'@importFrom crayon cyan magenta
#'@importFrom tibble as.tibble
#'@export
print.eidith_tbl <- function(x, tibble_print = TRUE, ...){
  if(tibble_print) print(as.tibble(unclass(x)), ...)
  note_cols <- attributes(x)$notes
  if(length(note_cols > 0)){
    cat_line("")
    cat_line(cyan("There are notes attached to this dataframe which may contain important information! Please look at the following column(s):"))
    cat_line(paste("    ", magenta(names(x)[note_cols])))
  }
  duplicate_keys <- attributes(x)$duplicate_keys
  duplicate_indices <- attributes(x)$duplicate_indices
  if(length(duplicate_keys > 0)){
    cat_line("")
    cat_line(red(paste0("IMPORTANT: There are ", length(duplicate_indices)," rows with duplicate unique ID's in this table!")))
    cat_line("              The duplicate IDs are:")
    cat_line(paste("                  ", red(duplicate_keys)))
    cat_line(paste0("              This affects the following ", length(duplicate_indices)," rows:"))
    cat_line(paste("                  ", red(duplicate_indices)))
  }
}


fix_classes <- function(table) {
  logical_cols <- names(table)[names(table) %in% logical_vars]
  date_cols <-  names(table)[names(table) %in% date_vars]
  datetime_cols <- names(table)[names(table)  %in% datetime_vars]
  if(length(logical_cols) != 0) table <- mutate_at(table, vars(logical_cols), funs_("as.logical"))
  if(length(date_cols) != 0) table <- mutate_at(table, vars(date_cols), funs(quicktime))
  if(length(datetime_cols) != 0) table <- mutate_at(table, vars(datetime_cols), funs(quicktime2))
  return(table)
}

#' @export
#' @rdname ed_table
ed_table <- function(table, ...) {
  ed_table_(table, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_events <- function(...) {
  ed_table_("events", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_animals <- function(...) {
  ed_table_("animals", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_specimens <- function(...) {
  ed_table_("specimens", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_tests <- function(...) {
  ed_table_("tests", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_testspecimen <- function(...) {
  ed_table_("test_specimen_ids", .dots = lazyeval::lazy_dots(...))
}


#' Load PREDICT-2 EIDITH data from the local database
#'
#' These functions load data from the locally-stored SQLite database of downloaded
#' and cleaned EIDITH data. `ed_table` is a general function, and `ed_table_`
#' it's [standard evaluation](https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html)
#' equivalent, useful for programming.  The other functions are convenience aliases
#' for the individual tables. Functions beginning with the `ed` prefix refer to
#' PREDICT-1 data tables, while `ed2` functions refer to PREDICT-2 data tables.
#'
#' These functions take [dplyr::filter()] arguments to sub-set the data.  Using
#' these, the data is subsetted via SQL *before* it is loaded into memory.
#' For large tables, such as the *tests* table, this is useful for reducing the memory footprint of your R session.
#'
#' Note that subsetting in SQL is more limited:
#'
#' -  Use `0` or `1` instead of `TRUE` or `FALSE`
#' -  Dates are stored as character strings, but as they are in YYYY-MM-DD
#'    format, filtering such as `event_date > "2014-01-01"` still works.
#'
#' @param ... arguments passed to [dplyr::filter()] to subset data
#' @return a [tibble][tibble::tibble]-style data frame.
#' @rdname ed2_table
#' @name ed2_table
#' @aliases ed2_events
#' @export
ed2_events <- function(...){
  ed_table_("events_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_animals <- function(...) {
  ed_table_("animals_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_specimens <- function(...) {
  ed_table_("specimens_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_animal_production <- function(...){
  ed_table_("animal_production_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_crop_production <- function(...){
  ed_table_("crop_production_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_zoo_sanctuary <- function(...){
  ed_table_("zoo_sanctuary_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_wildlife_restaurant <- function(...){
  ed_table_("wildlife_restaurant_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_natural_areas <- function(...){
  ed_table_("natural_areas_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_market_value_chain <- function(...){
  ed_table_("market_value_chain_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human <- function(...){
  ed_table_("human_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_ehp <- function(...){
  ed_table_("human_ehp_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_tests <- function(...){
  ed_table_("tests_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_test_serology <- function(...){
  ed_table_("test_data_serology_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_test_interpreted <- function(...){
  ed_table_("test_data_interpreted_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_animal_production <- function(...){
  ed_table_("human_animal_production_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_animal_production_ehp <- function(...){
  ed_table_("human_animal_production_ehp_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_extractive_industry <- function(...){
  ed_table_("extractive_industry_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_dwellings <- function(...){
  ed_table_("dwellings_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_crop_production <- function(...){
  ed_table("human_crop_production_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_extractive_industry <- function(...){
  ed_table_("human_extractive_industry_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_hospital_worker <- function(...){
  ed_table_("human_hospital_worker_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_hunter <- function(...){
  ed_table_("human_hunter_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_hunter_ehp <- function(...){
  ed_table_("human_hunter_ehp_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_market <- function(...){
  ed_table_("human_market_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_restaurant <- function(...){
  ed_table_("human_restaurant_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_sick_person <- function(...){
  ed_table_("human_sick_person_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_temporary_settlements <- function(...){
  ed_table_("human_temporary_settlements_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_human_zoo <- function(...){
  ed_table_("human_zoo_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_behavior <- function(...){
  ed_table_("behavioral_2", .dots = lazyeval::lazy_dots(...))
}

#' @rdname ed2_table
#' @export
ed2_training <- function(...){
  ed_table_("training_2", .dots = lazyeval::lazy_dots(...))
}
