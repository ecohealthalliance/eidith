#' Downloads EIDITH from google drive
#'
#' @param database whether downloading "global", "eha", or "eha_and_malaysia" database (all password protected)
#' @importFrom getPass getPass
#' @importFrom rappdirs user_data_dir
#' @importFrom assertthat assert_that
#' @importFrom googledrive drive_download
#' @return NULL
#' @export
import_local_db <- function(database = c("global", "eha", "eha_with_malaysia")){

  # establish paths
  drive_url <- switch(database, "global" = "https://drive.google.com/file/d/1BClZHx5WJV-8sO_vXyMoIvp7NfUZ2m2M/view?usp=sharing")
    #paste0("~/eidith/", database, "/eidith_db.zip")
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
