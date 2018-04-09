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
#'     tmp <- tempfile(fileext=".html")
#'       htmlwidgets::saveWidget(DT::datatable(eidith::ed_metadata()), tmp)
#'       mytext <- paste('Below is a searchable version of this table.',
#'       'Here \\\bold{original_name} refers to the name in the online EIDITH database. The variable name in the local database is \\\bold{auto_processed_name} or \\\bold{replacement_name}, if the latter exists. Variables dropped from the local database have "DROP" in \\\bold{replacement_name}. Where relevant \\\bold{processing_notes} describes transformations applied to the data in \\\code{ed_process()}.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     mytext
#' }
#' }
#
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
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'     tmp <- tempfile(fileext=".html")
#'       htmlwidgets::saveWidget(DT::datatable(eidith::ed2_metadata()), tmp)
#'       mytext <- paste('Below is a searchable version of this table.',
#'       'Here \\\bold{original_name} refers to the name in the online EIDITH database. The variable name in the local database is \\\bold{auto_processed_name} or \\\bold{replacement_name}, if the latter exists. Variables dropped from the local database have "DROP" in \\\bold{replacement_name}. Where relevant \\\bold{processing_notes} describes transformations applied to the data in \\\code{ed_process()} or information regarding the type of variable. The \\\bold{question} column is relevant for Human Questionnaire tables, indication which question the data field comes from.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     mytext
#' }
#' }
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

#' @importFrom readr read_csv
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
    print(e)
    return(NULL)
  }
  )
}


