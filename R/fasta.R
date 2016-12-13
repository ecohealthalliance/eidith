#' Export FASTA files
#'
#' `ed_fasta()` and `ed_fasta_()` convert a data frame to a FASTA file.  `ed_tests_report()` is a standard report returning a data frame of all uninterpreted test sequences, along with a set of metadata, which is commonly printed to FASTA
#'
#' @param .data The data frame
#' @param file name of file to write to. If "" (default), the output is written to console.
#' @param sequence the column name in the data frame that contains the sequence.  Default is "sequence".
#' @param ... Additional columns to include as metadata, using [dplyr::select()] syntax.  If nothing is included, all columns are used.
#'
#' @return The FASTA file is written to a file or the console.  Also, a character vector of each FASTA record is returned invisibly.  `ed_tests_report()` returns a data frame.
#' @export
#' @rdname ed_fasta
#' @aliases fasta
#' @examples
#' \dontrun{
#'  ed_fasta(ed_tests_report())
#'  }
ed_fasta <- function(.data, file = "", sequence="sequence", ...) {
  ed_fasta_(.data=.data, file=file, sequence=sequence, .dots = lazyeval::lazy_dots(...))
}

#' @param .dots Columns specified using standard evaluation.
#' @rdname ed_fasta
#' @export
#' @importFrom dplyr select_
ed_fasta_ <- function(.data, file="", sequence="sequence", ..., .dots) {
  if(!(sequence %in% names(.data))) {
    stop("Sequence column not found.")
  }
  seq = .data[[sequence]]
  .dots = .dots=lazyeval::all_dots(.dots, ...)
  if(length(.dots) == 0) {
    .dots = names(.data)[names(.data) != sequence]
  }
  meta = select_(.data, .dots=.dots)
  meta_str = purrr::map2(names(meta), meta, ~paste(.x, .y, sep=": "))
  meta_str = do.call("paste", args = c(meta_str, sep = " / "))
  meta_str = paste0(">", meta_str, "\n", seq)
  cat(meta_str, file=file, sep = "\n")
  invisible(meta_str)
}

#' @rdname ed_fasta
#' @export
#' @importFrom dplyr %>% left_join right_join group_by summarise
ed_tests_report <- function() {
  ## First we select tests for which interpretation isn't complete and aren't pool positives
  tests <- ed_table_("tests", .dots=c(~!(test_status %in% c("Interpretation completed", "Pooled test positive")),  ~!is.na(sequence)))
  # Then join together with other data to get other fields for metadata
  test_spec <- ed_table_("test_specimen_ids", ~test_id %in% tests[["test_id"]])
  spec <- ed_table_("specimens", ~specimen_id %in% test_spec[["specimen_id"]])
  anim <- ed_table_("animals", ~animal_id %in% spec[["animal_id"]])
  events <- ed_table_("events", ~event_id %in% anim[["event_id"]])
  left_join(tests, test_spec, by="test_id") %>%
    right_join(spec, by="specimen_id") %>%
    right_join(anim, by="animal_id") %>%
    right_join(events, by="event_id") %>%
    group_by(test_id) %>%
    ## These are the fields that end up in the metadata of the report
    summarise(specimen_ids = paste(unique(specimen_id), collapse=","),
              species = paste(unique(species_scientific_name), collapse=","),
              country = paste(unique(country), collapse=","),
              lab = paste(unique(diagnostic_laboratory_name), collapse=","),
              sequence=unique(sequence))
}
