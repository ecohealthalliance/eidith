#' EIDITH table metadata
#'
#' Descriptions of fields in the EIDITH database.  Running `ed_metadata()` will
#' return a data frame with this information. \if{html}{This table is also shown below in a searchable and exportable format.}
#'
#' More information can be found at the [EIDITH resources page](https://eidith.org/Resources.aspx).
#'
#' \if{html}{
#'
#' Here "Original Name" refers to the name in the EIDITH database, "Processed Name"
#' is what is found after running \code{\link{ed_process}} on the data tables.
#'
#'\Sexpr[echo=FALSE, results=rd, stage=render]{tmp <- tempfile(fileext=".html");htmlwidgets::saveWidget(DT::datatable(eidith::ed_metadata()), tmp);return(paste(c('\\\out{<div>', readLines(tmp), '</div>}'), collapse = "\n"))}}
#'
#' @rdname ed_metadata
#' @export
ed_metadata <- function() {
  readr::read_csv(system.file("ed_metadata.csv", package="eidith"), col_types="cccc")
}
