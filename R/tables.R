logical_vars <- c("archived_data", "deep_forest_data", "priortized_for_testing",
                  "pooled", "message_sent_to_country", "message_sent_to_govt",
                  "govt_approved_release", "predict_protocol", "known",
                  "known_human_risk")

#' @importFrom dplyr tbl tbl_df filter_
#' @export
ed_table_ <- function(table, .dots) {
  table <- tbl(eidith_db, table) %>%
    filter_(.dots=.dots) %>%
    tbl_df
  logical_cols <- logical_vars[logical_vars %in% names(table)]
  mutate_each_(table, funs(as.logical), logical_cols)
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
