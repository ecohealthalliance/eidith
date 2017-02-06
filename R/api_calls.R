eidith_base_url <- "https://predict2api.eidith.org/api/modeling/"
endpoints <- c("Event", "Animal", "Specimen", "Test", "Virus") #,
               #"TestIDSpecimenID")

#' Functions to download EIDITH tables via API
#'
#' These function download data directly from the EIDITH API.  They require
#' [authorization][ed_auth].  They can be useful for comparing local data
#' against data updated in the database, or processed vs. unprocessed data.
#' The [table functions][ed_table] load data from the local database instead,
#' and are thus faster and work without an internet connection.
#'
#' @param verbose Show a progress bar and other messages?
#' @param postprocess Should data be cleaned via [ed_process()] or returned raw?
#' @param header_only Return only the table header.  Useful for checking if
#' API access works
#' @param lmdate_from filter records by earliest date, in YYYY-MM-DD format
#' @param lmdate_to filter records by latest date, in YYYY-MM-DD format (see details)
#' @param ... additional arguments passed to [httr::GET()]
#' @return a [tibble][tibble::tibble()]-style data frame
#' @rdname ed_get
#' @name ed_get
NULL


#' @noRd
#' @importFrom httr GET status_code progress authenticate content modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
ed_get <- function(endpoint, verbose=interactive(), postprocess=TRUE,
                   header_only=FALSE, lmdate_from="2000-01-01",
                   lmdate_to=Sys.Date() + 1, auth=NULL, ...) {
  url <- modify_url(url =  paste0(eidith_base_url, endpoint),
                    query = list(header_only = ifelse(header_only, "y", "n"),
                                 lmdate_from = lmdate_from,
                                 lmdate_to = lmdate_to))
  if(verbose) {
    pbar <- progress()
    message(paste("Downloading", endpoint, "table..."))
  } else {
    pbar <- NULL
  }

  if(is.null(auth)) auth <- ed_auth()

  request <- GET(url=url, authenticate(auth[1], auth[2], type="basic"),
                 pbar, ...)

  if(status_code(request) == 401) {
    stop("Unauthorized (HTTP 401). Your username or password do not match an account.
See ?ed_auth.")
  }

  if(status_code(request) == 403) {
    stop("Forbidden (HTTP 403). Your account does not have access permissions.
Contact technology@eidith.org.")
  }

  if(verbose) message("Importing...")
  data <- fromJSON(content(request, as = "text", encoding="UTF-8"))

  if(header_only) {
    return(data)
  } else {
    data <- as_tibble(data)
  }

  if(postprocess) data <- ed_process(data, endpoint)

  return(data)
}

#' @rdname ed_get
#' @export
ed_get_events <- function(verbose=interactive(), postprocess=TRUE,
                          header_only=FALSE, lmdate_from="2000-01-01",
                          lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Event", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_animals <- function(verbose=interactive(), postprocess=TRUE,
                           header_only=FALSE, lmdate_from="2000-01-01",
                           lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Animal", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_specimens <- function(verbose=interactive(), postprocess=TRUE,
                             header_only=FALSE, lmdate_from="2000-01-01",
                             lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Specimen", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_tests <- function(verbose=interactive(), postprocess=TRUE,
                         header_only=FALSE, lmdate_from="2000-01-01",
                         lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Test", verbose, postprocess, header_only, lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_viruses <- function(verbose=interactive(), postprocess=TRUE,
                           header_only=FALSE, lmdate_from="2000-01-01",
                           lmdate_to=Sys.Date() + 1, ...) {
  ed_get("Virus", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}

#' @rdname ed_get
#' @export
ed_get_testspecimen <- function(verbose=interactive(), postprocess=TRUE,
                                header_only=FALSE, lmdate_from="2000-01-01",
                                lmdate_to=Sys.Date() + 1, ...) {
  ed_get("TestIDSpecimenID", verbose, postprocess, header_only,
         lmdate_from, lmdate_to, ...)
}
