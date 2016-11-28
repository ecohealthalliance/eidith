logical_vars <- c("archived_data", "deep_forest_data", "priortized_for_testing",
                  "pooled", "message_sent_to_country", "message_sent_to_govt",
                  "govt_approved_release", "predict_protocol", "known",
                  "known_human_risk")

date_vars <- c("event_date", "sample_date", "specimen_date",
               "test_date", "lab_submission_date", "results_date")

#' @importFrom dplyr tbl tbl_df filter_ mutate_each_ funs_ funs collect
#' @export
ed_table_ <- function(table, .dots) {
  tbl(eidith_db, table) %>%
    filter_(.dots=.dots) %>%
    collect(n=Inf) %>%
    fix_classes()
}

fix_classes <- function(table) {
  logical_cols <- names(table)[names(table) %in% logical_vars]
  date_cols <-  names(table)[names(table) %in% date_vars]
  datetime_cols <- names(table)[names(table) == "date_last_updated"]
  if(length(logical_cols) != 0) table <- mutate_each_(table, funs_("as.logical"), logical_cols)
  if(length(date_cols) != 0) table <- mutate_each_(table, funs(quicktime), date_cols)
  if(length(datetime_cols) != 0) table <- mutate_each_(table, funs(quicktime2), datetime_cols)
  return(table)
}

#' @export
ed_table = function(table, ...) {
  ed_table_(table, .dots = lazyeval::lazy_dots(...))
}

#' @export
ed_events = function(...) {
  ed_table_("events", .dots = lazyeval::lazy_dots(...))
}

#' @export
ed_animals = function(...) {
  ed_table_("animals", .dots = lazyeval::lazy_dots(...))
}

#' @export
ed_specimens = function(...) {
  ed_table_("specimens", .dots = lazyeval::lazy_dots(...))
}

#' @export
ed_tests = function(...) {
  ed_table_("tests", .dots = lazyeval::lazy_dots(...))
}

#' @export
ed_viruses = function(...) {
  ed_table_("viruses", .dots = lazyeval::lazy_dots(...))
}
