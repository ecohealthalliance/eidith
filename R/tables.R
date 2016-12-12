logical_vars <- c("archived_data", "deep_forest_data", "prioritized_for_testing",
                  "pooled", "message_sent_to_country", "message_sent_to_govt",
                  "govt_approved_release", "predict_protocol", "known",
                  "known_human_risk")

date_vars <- c("event_date", "sample_date", "specimen_date",
               "test_date", "lab_submission_date", "results_date")

datetime_vars <- c("date_created", "date_modified", "database_date")

#' Load EIDITH data from the local database
#'
#' These functions load data from the locally-stored SQLite database of downloaded
#' and cleaned EIDITH data. `ed_table` is a general function, and `ed_table_`
#' it's [standard evaluation](https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html)
#' equivalent, useful for programming.  The other functions are convenience aliases
#' for the individual tables.
#'
#' These functions take [dplyr::filter()] arguments to sub-set the data.  Using
#' these, the data is subsetted via SQL *before* it is loaded into memory.
#' For large tables, such as the *tests* table, this is useful for reducing the memory footprint of your R session.
#'
#' @param table one of the EIDITH database tables. One of "events", "animals",
#' "specimens", "tests", "viruses", or "test_specimen_ids".
#' @param ... arguments passed to [dplyr::filter()] to subset data
#' @param .dots standard-evaluation versions of subsetting arguments
#' @return a [tibble][tibble::tibble]-style data frame.
#' @importFrom dplyr tbl tbl_df filter_ mutate_each_ funs_ funs collect
#' @export
#' @rdname ed_table
ed_table_ <- function(table, .dots) {
  tbl(eidith_db(), table) %>%
    filter_(.dots=.dots) %>%
    collect(n=Inf) %>%
    fix_classes()
}

fix_classes <- function(table) {
  logical_cols <- names(table)[names(table) %in% logical_vars]
  date_cols <-  names(table)[names(table) %in% date_vars]
  datetime_cols <- names(table)[names(table)  %in% datetime_vars]
  if(length(logical_cols) != 0) table <- mutate_each_(table, funs_("as.logical"), logical_cols)
  if(length(date_cols) != 0) table <- mutate_each_(table, funs(quicktime), date_cols)
  if(length(datetime_cols) != 0) table <- mutate_each_(table, funs(quicktime2), datetime_cols)
  return(table)
}

#' @export
#' @rdname ed_table
ed_table = function(table, ...) {
  ed_table_(table, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_events = function(...) {
  ed_table_("events", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_animals = function(...) {
  ed_table_("animals", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_specimens = function(...) {
  ed_table_("specimens", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_tests = function(...) {
  ed_table_("tests", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_viruses = function(...) {
  ed_table_("viruses", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_table
ed_testspecimen = function(...) {
  ed_table_("test_specimen_ids", .dots = lazyeval::lazy_dots(...))
}
