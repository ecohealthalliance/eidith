#' Downloads EIDITH database from google drive
#'
#' Note that post-September 2020, EIDITH remote database access is disabled.
#'
#' This function downloads a static version of the database. Access is enforced via Google Drive.
#' You will be prompted in your R console to allow the `googledrive` package to access to your Google account.
#' See `?googledrive::drive_auth()`.
#'
#' Contact Emma (mendelsohn@ecohealthalliance.org) for access or with any questions!
#'
#' @param database whether downloading "eha", "eha_and_malaysia",or "glabal" database
#' @importFrom rappdirs user_data_dir
#' @importFrom googledrive drive_download
#' @return NULL
#' @export
import_local_db <- function(database = c("eha", "eha_with_malaysia", "global")){

  database = match.arg(database)

  # establish paths
  drive_url <- switch(database,
                      "global" = "https://drive.google.com/file/d/1i_7zJvPEhKQ0D8YGnqZpBsEjP8cYoFUB/view?usp=sharing",
                      "eha" = "https://drive.google.com/file/d/1Hu4hjsJepMAQ03XnzAYWpdFdqYg_4JfV/view?usp=sharing",
                      "eha_with_malaysia" = "https://drive.google.com/file/d/1zBXavCFg9fvwdPVQqLfCW3aDWWjYraDU/view?usp=sharing"
                      )
  local_path <- file.path(rappdirs::user_data_dir(),
                          "eidith")
  local_path_zip <- paste0(local_path,  "/eidith_db.sqlite.zip")

  #save to eidith local share folder from google drive
  googledrive::drive_download(file = drive_url,
                 path = local_path_zip,
                 overwrite = TRUE)

  # unzip
  unzip(zipfile = local_path_zip, exdir = local_path)

  return(local_path)
}

