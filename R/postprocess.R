#' Standard post-processing of EIDITH exports
#'
#' This function takes raw EIDITH data tables and does minor common
#' post-processing tasks.
#'
#' @param data The data as exported from EIDITH and imported via
#' @param endpoint The name of the endpoint or data table: one of "Event",
#' "Animal",  "Specimen", "Test", or "Virus".
#'
ed_postprocess <- function(data, endpoint) {
  names(data) = camel_to_snake(names(data))

  return(data)
}


camel_to_snake <- function(CamelNames) {
  Snake__names <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", CamelNames, perl = TRUE)
  snake__names <- tolower(Snake__names)
  snake_names <- gsub(" ", "_", snake__names, fixed=TRUE)
  snake_names <- gsub("__", "_", snake_names, fixed=TRUE)
  return(snake_names)
}

