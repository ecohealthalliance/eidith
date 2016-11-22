eidith_base_url <- "https://predict2api.eidith.org/api/app/"
endpoints <- c("Event", "Animal", "Specimen", "Test", "Virus", "TestIDSpecimenID")
#' Functions to access main EIDITH tables
#' @param endpoint Which API endpoint to access, one of "Event", "Animal",
#'   "Specimen", "Test", or "Virus". Each endpoint delivers one of these tables.
#'   Convenience functions are provided for each.
#' @param verbose Show a progress bar and other messages?
#' @return a \link[tibble]{tibble}-style data frame
#' @importFrom httr GET status_code progress authenticate content modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
ed_get <- function(endpoint, verbose=interactive(), postprocess=TRUE, header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  url <- modify_url(url =  paste0(eidith_base_url, endpoint),
                    query = list(header_only = ifelse(header_only, "y", "n"),
                                 lmdate_from = lmdate_from,
                                 lmdate_to = lmdate_to))
  if(verbose) {
    pbar = progress()
    message("Downloading...")
  } else {
    pbar=NULL
  }

  if(is.null(auth)) auth <- eidith_auth(username, password)

  request <- GET(url=url, authenticate(auth[1], auth[2], type="basic"), pbar, ...)

  if(status_code(request) == 401) {
    stop("Unauthorized (HTTP 401). See ?eidith_auth.")
  }

  if(verbose) message("Importing...")
  data <- fromJSON(content(request, as = "text", encoding="UTF-8"))

  if(header_only) {
    return(data)
  } else {
    data = as_tibble(data)
  }

  if(postprocess) data = ed_postprocess(data, endpoint)

  return(data)
}

#' @rdname ed_get
#' @export
ed_events <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Event", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_animals <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Animal", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_specimens <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Specimen", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_tests <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Test", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_viruses <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Virus", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_testspecimen <- function(verbose=interactive(), postprocess=TRUE,  header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
  ed_get("TestIDSpecimenID", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}
