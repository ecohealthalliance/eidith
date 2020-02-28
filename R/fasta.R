#' Export FASTA files for viral interpretation
#'
#' @description `ed_fasta()` and `ed_fasta_group()` convert a data frame to a FASTA file.  `ed_tests_report()` is a standard report returning a data frame of all uninterpreted test sequences, along with a set of metadata, which is commonly printed to FASTA.
#'  `ed_report_excel()` is a convenience function for creating excel-ready CSVs for reporting.  See the [FASTA vignette](https://ecohealthalliance.github.io/eidith/articles/fasta_walkthrough.html) for more guidance.
#'
#' @param .data The data frame
#' @param filename name of file to write to. If "" (default), the output is written to console.
#' @param sequence the column name in the data frame that contains the sequence.  Default is "sequence".
#' @param include_names Whether to include column names of sequence discriptors.  Default is TRUE.
#' @param ... Additional columns to include as metadata, using [dplyr::select()] syntax.  If nothing is included, all columns are used.
#' @return For `ed_fasta()`, `ed_fasta_group()`, and `ed_report_excel()`, the file(s) will be written to a file or the console (default).  Also, a character vector of each FASTA record is returned invisibly
#' `ed_tests_report()` returns a data frame.
#' @importFrom dplyr select_
#' @export
#' @rdname ed_fasta
#' @aliases fasta
#' @examples
#' \dontrun{
#'  ed_fasta(ed_tests_report())
#'  }
ed_fasta <- function(.data, filename="", sequence="sequence", include_names = TRUE, ...) {
  .dots = lazyeval::lazy_dots(...)
  if(!(sequence %in% names(.data))) {
    stop("Sequence column not found.")
  }
  seq = .data[[sequence]]
  if(length(.dots) == 0) {
    .dots = names(.data)[names(.data) != sequence]
  }
  meta = select_(.data, .dots=.dots)
  if(include_names){
    meta_str = purrr::map2(names(meta), meta, ~paste(.x, .y, sep="="))
  }else{
    meta_str = purrr::map(meta, ~.x)
  }
  meta_str = do.call("paste", args = c(meta_str, sep = "|"))
  meta_str = paste0(">", meta_str)
  meta_str = stri_replace_all_fixed(meta_str, " ", "_")
  meta_str = paste0(meta_str, "\n", seq)
  cat(meta_str, file=filename, sep = "\n")
  invisible(meta_str)
}
