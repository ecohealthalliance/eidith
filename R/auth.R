#' EIDITH logon credentials
#
#' Any function that downloads data from EIDITH, such as [download_db()] or
#' the [raw download functions][ed_get()] requires login credentials.  These
#' functions will ask for a username and password.
#'
#' For frequent or programmatic use, we recommend caching your logon credentials
#' as environment variables.  **eidith** functions will automatically search
#' for your credentials in the `EIDITH_USERNAME` and `EIDITH_PASSWORD` environment
#' variables.
#'
#' To cache your credentials across multiple R sessions, save them in your
#' personal `.Renviron` file by running the following code in your R console
#' (replacing `YOUR_USERNAME` and `YOUR_PASSWORD` with actual values).
#'
#' ```
#' cat("EIDITH_USERNAME=YOUR_USERNAME\nEIDITH_PASSWORD=YOUR_PASSWORD",
#'     file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
#' ```
#'
#' Restart R, and you will be automatically logged in to download data from
#' when working on this computer.
#'
#' @rdname eidith_auth
#' @name eidith_auth
NULL

#' @param verbose Show messages?
#' @param force Ask for logon credentials even if environment variables are
#' provided.
#' @noRd
eidith_auth <- function(verbose=interactive(), force=FALSE) {
  user <- Sys.getenv("EIDITH_USERNAME")
  pwd <- Sys.getenv("EIDITH_PASSWORD")

  if(identical(user, "") || identical(pwd, "") || force) {
    if(interactive()) {
      message("We recommend saving EIDITH credentials as environment variables.See ?eidith_auth")
      user <- readline("EIDITH username: ")
      pwd <- getPass::getPass("EIDITH password: ")
    } else {
      stop("No credentials supplied. See ?eidith_auth.")
    }
  } else {
    if (verbose) message("Using env vars EIDITH_USERNAME and EIDITH_PASSWORD for logon")
  }

  auth <- c(username=user, password=pwd)
  class(auth) <- c(class(auth), "eidithauth")
  invisible(auth)
}

# print.eidithauth <- function(auth) {
#   if(interactive()) message("Credentials hidden")
# }
#
# str.eidithauth <- function(auth) {
#   stop("Operation not allowed")
# }
#
# `[.eidithauth` <- function(auth, n) {
#   stop("Operation not allowed")
# }
