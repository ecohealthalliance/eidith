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
get_eidith_data <- function(endpoint, username=eidith_user(), password=eidith_pwd(), progress=interactive()) {
   url <- paste0(predict_base_url, endpoint)
   if(progress) pbar = progress() else pbar=NULL
   request <- GET(url=url, authenticate(username, password, type="basic"), pbar)
   if(status_code(request) != 200) {
     stop(paste("Requested failed with HTTP code", status_code(request)))
   }
   data <- as_tibble(fromJSON(content(request, as = "text", encoding="UTF-8")))
   names(data) <-camel_to_snake(names(data))
   return(data)
}

#' @rdname get_eidith_data
#' @export
ed_events <- function(username=eidith_user(), password=eidith_pwd()) {
  get_eidith_data("Event", username, password)
}

#' @rdname get_eidith_data
#' @export
ed_animals <- function(username=eidith_user(), password=eidith_pwd()) {
  get_eidith_data("Animal", username, password)
}

#' @rdname get_eidith_data
#' @export
ed_specimens <- function(username=eidith_user(), password=eidith_pwd()) {
  get_eidith_data("Specimen", username, password)
}

#' @rdname get_eidith_data
#' @export
ed_tests <- function(username=eidith_user(), password=eidith_pwd()) {
  get_eidith_data("Test", username, password)
}

#' @rdname get_eidith_data
#' @export
ed_viruses <- function(username=eidith_user(), password=eidith_pwd()) {
  get_eidith_data("Virus", username, password)
}
