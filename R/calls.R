eidith_base_url <- "https://predict2api.eidith.org/api/app/"
endpoints <- c("Event", "Animal", "Specimen", "Test", "Virus", "TestIDSpecimenID")
#' Functions to access main EIDITH tables
#' @param endpoint Which API endpoint to access, one of "Event", "Animal",
#'   "Specimen", "Test", or "Virus". Each endpoint delivers one of these tables.
#'   Convenience functions are provided for each.
#' @param username  Your EIDITH username. Store it in your \code{.Renviron} file
#'   as \code{EIDITH_USERNAME=XXXXX} for automatic access.
#' @param username  Your EIDITH password. Store it in your \code{.Renviron} file
#'   as \code{EIDITH_PASSWORD=XXXXX} for automatic access.
#' @param verbose Show a progress bar and other messages?
#' @return a \link[tibble]{tibble}-style data frame
#' @importFrom httr GET status_code progress authenticate content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
ed_get <- function(endpoint, username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, header_only=FALSE, lmdate_from="2000-01-01", lmdate_to=Sys.Date() + 1, ...) {
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

   if(is.null(password)) password = eidith_pwd(verbose)

   request <- GET(url=url, authenticate(username, password, type="basic"), pbar, ...)

   if(status_code(request) != 200) {
     stop(paste("Requested failed with HTTP code", status_code(request)))
   }
   if(verbose) message("Importing...")
   data <- as_tibble(fromJSON(content(request, as = "text", encoding="UTF-8")))

   if(postprocess) data = ed_postprocess(data, endpoint)

   return(data)
}

#' @rdname ed_get
#' @export
ed_events <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("Event", username, password, verbose, postprocess, ...)
}

#' @rdname ed_get
#' @export
ed_animals <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("Animal", username, password, verbose, postprocess, ...)
}

#' @rdname ed_get
#' @export
ed_specimens <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("Specimen", username, password, verbose, postprocess, ...)
}

#' @rdname ed_get
#' @export
ed_tests <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("Test", username, password, verbose, postprocess, ...)
}

#' @rdname ed_get
#' @export
ed_viruses <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("Virus", username, password, verbose, postprocess, ...)
}

#' @rdname ed_get
#' @export
ed_testspecimen <- function(username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
  ed_get("TestIDSpecimenID", username, password, verbose, postprocess, ...)
}
