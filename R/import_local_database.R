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
  drive_url <- switch(database,
                      "global" = "https://drive.google.com/file/d/1fWr3pEMTWvSatf3z9ecBAYXnZnDViXlS/view?usp=sharing",
                      "eha" = "https://drive.google.com/file/d/1QeCv-9oUtgrUakGkS5ccK7CXXKZFRqwF/view?usp=sharing",
                      "eha_with_malaysia" = "https://drive.google.com/file/d/1PbFO7-FhSkd9cBin5RP1r8SMDYrQdgMO/view?usp=sharing"
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
