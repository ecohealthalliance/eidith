#' EIDITH logon credentials
#'
#' These are convience functions for using your default EIDITH logon credentials
#' for any calls to EIDITH.  Place your EIDITH username and password in your
#' home \code{.Renviron} file as \code{EIDITH_USERNAME} and
#' \code{EIDITH_PASSWORD} and these values will be used as defaults for all
#' calls.
#' @param verbose Show messages?
#' @export
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

print.eidithauth <- function(auth) {
  if(interactive()) message("Credentials hidden")
}

str.eidithauth <- function(auth) {
  stop("Operation not allowed")
}

`[.eidithauth` <- function(auth, n) {
  stop("Operation not allowed")
}
