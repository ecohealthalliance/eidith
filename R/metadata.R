#' EIDITH table metadata
#'
#' Descriptions of fields in the EIDITH database.  Running `ed_metadata()` will
#' return a data frame with this information.
#'
#' More information can be found at [the EIDITH resources page](https://eidith.org/Resources.aspx).
#'
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       text <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'     tmp <- tempfile(fileext=".html")
#'       htmlwidgets::saveWidget(DT::datatable(eidith::ed_metadata()), tmp)
#'       text <- paste('Below is a searchable version of this table.',
#'       'Here "Original Name" refers to the name in the online EIDITH database, "Processed Name" is the name in the locally-stored tables after processing.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     text
#' }
#' }
#
#' @rdname ed_metadata
#' @export
ed_metadata <- function() {
 ed_metadata_
}
