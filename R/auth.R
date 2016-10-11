#' EIDITH logon credentials
#'
#' These are convience functions for using your default EIDITH logon credentials
#' for any calls to EIDITH.  Place your EIDITH username and password in your
#' home \code{.Renviron} file as \code{EIDITH_USERNAME} and
#' \code{EIDITH_PASSWORD} and these values will be used as defaults for all
#' calls.
#' @param quiet Suppress messages when calling values
#' @export

eidith_user <- function(quiet=!interactive()) {
  user <- Sys.getenv("EIDITH_USERNAME")
  if(identical(user, "")) return(NULL)
  if (!quiet) message("Using EIDITH username from envar EIDITH_USERNAME")
  return(user)
}

eidith_pwd <- function(quiet=!interactive()) {
  pwd <- Sys.getenv("EIDITH_PASSWORD")
  if (!quiet) message("Using EIDITH password from envar EIDITH_PASSWORD")
  return(pwd)
}

