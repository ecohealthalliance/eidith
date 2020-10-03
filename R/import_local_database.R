#' Downloads EIDITH from google drive
#'
#' @param database whether downloading "global", "eha", or "eha_and_malaysia" database (all password protected)
#' @importFrom getPass getPass
#' @importFrom rappdirs user_data_dir
#' @importFrom assertthat assert_that
#' @importFrom googledrive drive_download
#' @return NULL
#' @export
import_local_db <- function(database = c("global", "eha", "eha_and_malaysia")){

  # establish paths
  drive_path <- paste0("~/eidith/", database, "/eidith_db.zip")
  local_path <- file.path(rappdirs::user_data_dir(),
                          "eidith")
  local_path_zip <- paste0(local_path,  "/eidith_db.zip")

  #save to eidith local share folder from google drive
  drive_download(file = drive_path,
                 path = local_path_zip,
                 overwrite = TRUE)

  # get unzip password
  pw <- Sys.getenv(paste(toupper(database), "EIDITH", "PASSWORD", sep = "_"))
  if(pw == ""){
    pw <- getPass::getPass(paste0("EIDITH ", database, " password"))
  }

  # unzip
  system(command = paste0("unzip -o -P ", pw, " ", local_path_zip, " -d ", local_path), wait = TRUE )

  assertthat::assert_that(file.exists(paste0(local_path, "/eidith_db.zip")))
  return("complete")
}
