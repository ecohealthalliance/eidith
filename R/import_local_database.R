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
  drive_url <- switch(database, "global" = "https://drive.google.com/file/d/1LC_i7jba6kbwMcIa3n_MNu8iQnvF1gsP/view?usp=sharing")
    #paste0("~/eidith/", database, "/eidith_db.zip")
  local_path <- file.path(rappdirs::user_data_dir(),
                          "eidith")
  local_path_zip <- paste0(local_path,  "/eidith_db.zip")

  #save to eidith local share folder from google drive
  googledrive::drive_download(file = drive_url,
                 path = local_path_zip,
                 overwrite = TRUE)

  # get unzip password
  pw <- Sys.getenv(paste(toupper(database), "EIDITH", "PASSWORD", sep = "_"))
  if(pw == ""){
    pw <- getPass::getPass(paste0("EIDITH ", database, " password"))
  }

  # unzip
  local_path <- paste0("'", local_path, "'")
  local_path_zip <- paste0("'", local_path_zip, "'")
  system(command = paste0("unzip -o -P ", pw, " ", local_path_zip, " -d ", local_path), wait = TRUE )
  return("complete")
}
