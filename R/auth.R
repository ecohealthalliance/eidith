#' EIDITH logon credentials
#'
#' Any function that downloads data from EIDITH, such as [ed_db_download()] or
#' the [raw download functions][ed_get()] requires login credentials.  These
#' functions will ask for a username and password.  To get an EIDITH account
#' with data access permissions, contact technology@eidith.org.
#'
#' For frequent or programmatic use, we recommend caching your logon credentials
#' as environment variables.  **eidith** functions will automatically search
#' for your credentials in the `EIDITH_USERNAME` and `EIDITH_PASSWORD` environment
#' variables.
#'
#' To cache your credentials across multiple R sessions, save them in your
#' personal `.Renviron` file. You can also use the `usethis` package to easily access
#' and edit your `.Renviron` file:
#'
#' ```
#' install.packages("usethis")`
#' usethis::edit_r_environ()
#' ```
#'
#' This should open up your `.Renviron` file in a new tab, and you add the lines:
#'
#' ```
#' EIDITH_USERNAME=your_username
#' EIDITH_PASSWORD=your_password
#' ```
#'
#' Save this file, restart R, and you will be automatically logged in to download data from EIDITH
#' when working on this computer.
#'
#' @rdname ed_auth
#' @name ed_auth
NULL

#' @param verbose Show messages?
#' @param force Ask for logon credentials even if environment variables are
#' provided.
#' @noRd
ed_auth <- function(verbose=interactive(), force=FALSE) {
  user <- Sys.getenv("EIDITH_USERNAME")
  pwd <- Sys.getenv("EIDITH_PASSWORD")

  if(identical(user, "") || identical(pwd, "") || force) {
    if(interactive()) {
      message("We recommend saving EIDITH credentials as environment variables. See ?ed_auth")
      user <- readline("EIDITH username: ")
      pwd <- getPass::getPass("EIDITH password: ")
    } else {
      stop("No credentials supplied. See ?ed_auth.")
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
