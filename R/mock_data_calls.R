#' Load mock EIDITH data for tutorials
#'
#' These functions load mock versions of the EIDITH data tables to be used
#' for demonstrations, tutorials, and practice.  They are drop-in replacements
#' for the [ed_table()] functions.
#'
#' @param mock_table one of the EIDITH database tables. One of "events", "animals",
#' "specimens", "tests", "viruses", or "test_specimen_ids".
#' @param ... arguments passed to [dplyr::filter()] to subset data
#' @param .dots standard-evaluation versions of subsetting arguments
#' @param raw Whether to return the unprocessed form of the mock data.  This
#' is equivalent to calling the [ed_get()] functions with `postprocess=FALSE`
#' @return a [tibble][tibble::tibble]-style data frame.
#' @importFrom dplyr tbl tbl_df filter_
#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_table_mock_ <- function(mock_table, ..., .dots, raw=FALSE) {
  dots <- lazyeval::all_dots(.dots, ...)
  if(raw) {
    data_list <- raw_mock_data
  } else {
    data_list <- processed_mock_data
  }
  filter_(data_list[[mock_table]], .dots=dots) %>%
    fix_classes()
}


#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_table_mock <- function(mock_table, ...) {
  ed_table_mock_(mock_table, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_events_mock <- function(...) {
  ed_table_mock_("events", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_animals_mock <- function(...) {
  ed_table_mock_("animals", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_specimens_mock  <- function(...) {
  ed_table_mock_("specimens", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_tests_mock <- function(...) {
  ed_table_mock_("tests", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_viruses_mock <- function(...) {
  ed_table_mock_("viruses", .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname ed_mock
#' @aliases ed_mock
ed_testspecimen_mock <- function(...) {
  ed_table_mock_("testspecimen", .dots = lazyeval::lazy_dots(...))
}
