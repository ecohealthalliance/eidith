predict_base_url <- "https://predict2api.eidith.org/api/app/"

#' Functions to access main EIDITH tables
#' @param endpoint Which API endpoint to access, one of "Event", "Animal",
#'   "Specimen", "Test", or "Virus". Each endpoint delivers one of these tables.
#'   Convenience functions are provided for each.
#' @param username  Your EIDITH username. Store it in your \code{.Renviron} file
#'   as \code{EIDITH_USERNAME=XXXXX} for automatic access.
#' @param username  Your EIDITH password. Store it in your \code{.Renviron} file
#'   as \code{EIDITH_PASSWORD=XXXXX} for automatic access.
#' @param progress Show a progress bar?
#' @return a \link[tibble]{tibble}-style data frame
#' @importFrom httr GET status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
ed_get <- function(endpoint, username=NULL, password=NULL, verbose=interactive(), postprocess=TRUE, ...) {
   url <- paste0(predict_base_url, endpoint)
   if(verbose) {
     pbar = progress()
     message("Downloading...\n")
     } else {
       pbar=NULL
     }

   if(is.null(username)) username = eidith_user(verbose)
   if(is.null(password)) password = eidith_pwd(verbose)

   request <- GET(url=url, authenticate(username, password, type="basic"), pbar, ...)

   if(status_code(request) != 200) {
     stop(paste("Requested failed with HTTP code", status_code(request)))
   }
   if(verbose) message("Importing...\n")
   data <- as_tibble(fromJSON(content(request, as = "text", encoding="UTF-8")))

   if(post_process) data = ed_postprocess(data, endpoint)

   return(data)
}

#' @rdname ed_get
#' @export
ed_events <- function(username=NULL, password=NULL, verbose=interactive(), post_process=TRUE) {
  ed_get("Event", username, password, verbose, post_process)
}

#' @rdname ed_get
#' @export
ed_animals <- function(username=NULL, password=NULL, verbose=interactive(), post_process=TRUE) {
  ed_get("Animal", username, password, verbose, post_process)
}

#' @rdname ed_get
#' @export
ed_specimens <- function(username=NULL, password=NULL, verbose=interactive(), post_process=TRUE) {
  ed_get("Specimen", username, password, verbose, post_process)
}

#' @rdname ed_get
#' @export
ed_tests <- function(username=NULL, password=NULL, verbose=interactive(), post_process=TRUE) {
  ed_get("Test", username, password, verbose, post_process)
}

#' @rdname ed_get
#' @export
ed_viruses <- function(username=NULL, password=NULL, verbose=interactive(), post_process=TRUE) {
  ed_get("Virus", username, password, verbose, post_process)
}
