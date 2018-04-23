#' EIDITH PREDICT-1 table metadata
#'
#' Descriptions of fields in the EIDITH database.  Running `ed_metadata()` will
#' return a data frame with this information. `ed_taxagroups` returns a table
#' of taxagroup-to-order mapping.
#'
#' More information can be found at [the EIDITH resources page](https://eidith.org/Resources.aspx).
#'
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'       mytext <- eidith::rd_datatable(eidith::ed_metadata())
#'     }
#'     mytext
#'   }
#' }
#'
#' \if{text,latex}{The HTML version of this help file includes a searchable table of values.}
#'
#' @rdname ed_metadata
#' @export
#' @importFrom DT datatable
#' @importFrom htmlwidgets saveWidget
ed_metadata <- function() {
  ed_metadata_
}


#' EIDITH PREDICT-2 table metadata
#'
#' Descriptions of fields in the EIDITH database.  Running `ed2_metadata()` will
#' return a data frame with this information.
#'
#' More information can be found at [the EIDITH resources page](https://eidith.org/Resources.aspx).
#'
#'
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'       mytext <- eidith::rd_datatable(eidith::ed2_metadata())
#'     }
#'     mytext
#'   }
#' }
#'
#' \if{text,latex}{The HTML version of this help file includes a searchable table of values.}
#'
#' @rdname ed2_metadata
#' @export
#' @importFrom DT datatable
#' @importFrom htmlwidgets saveWidget
ed2_metadata <- function() {
  out <- ed2_update_metadata()
  if(is.null(out)){
    return(ed2_metadata_)
  }else{
    return(out)
  }
}

#' @export
#' @rdname ed_metadata
ed_taxagroups <- function() {
  ed_taxagroups_
}

#' @importFrom readr read_csv cols col_character col_integer
ed2_update_metadata <- function(verbose = TRUE){

    updated_metadata <-
      tryCatch({readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQjx33e5jWhurOkHXIxgk7Hu8-5aVybj4lW0-vS5b1R3X2J0OR1tkFB5fkOID0o0hHQGIJShWMudhCk/pub?gid=983692865&single=true&output=csv",
                                 col_types = cols(
                                   table = col_character(),
                                   endpoint2 = col_character(),
                                   order = col_integer(),
                                   original_name = col_character(),
                                   auto_processed_name = col_character(),
                                   replacement_name = col_character(),
                                   description = col_character(),
                                   processing_notes = col_character(),
                                   question = col_character()
                                 ))
  }, error = function(e){
    if(verbose) cat("Automatic update of PREDICT-2 EIDITH metadata failed, using cached version.\nCheck your internet connection or see ?ed_contact for support if you continue to get this message.\n")
    #print(e)
    return(NULL)
  }
  )
}


#'@export
#'@importFrom DT datatable
#'@noRd
rd_datatable <- function(df, width="100%", ...) {
  wrap_widget(datatable(df, width=width, ...))
}

#'@export
#'@importFrom stringi stri_subset_regex
#'@importFrom htmlwidgets saveWidget
#'@noRd
wrap_widget <- function(widget) {
  tmp <- tempfile(fileext=".html")
  saveWidget(widget, tmp)
  widg <- paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html|head|title)",negate=TRUE), collapse="\n")
  paste('\\out{', escape_rd(widg), '}\n', sep="\n")
}

#'@export
#'@importFrom stringi stri_replace_all_fixed
#'@noRd
escape_rd <- function(x) {
  stri_replace_all_fixed(
    stri_replace_all_fixed(
      stri_replace_all_fixed(
        stri_replace_all_fixed(x, "\\", "\\\\"),
        "%", "\\%"),
      "{", "\\{"),
    "}", "\\}")
}

