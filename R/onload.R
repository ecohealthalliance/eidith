eidith_db <- NULL # need to pre-create this  so we can assign to it in .onLoad

.onLoad <- function(libname, pkgname) {
  eidith_db_path <- file.path(rappdirs::user_data_dir(), "eidith")
  eidith_db_file <-
    if (!dir.exists(eidith_db_path)) dir.create(eidith_db_path)
  # eidith_db <<- DBI::dbConnect(RSQLite::SQLite(), #assigning to the pkg env
  #                               file.path(eidith_db_path, "eidith_db.sqlite"))
  eidith_db <<- dplyr::src_sqlite(file.path(eidith_db_path, "eidith_db.sqlite"),
                                  create=TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Some message about db status") #Describe current countries in db, update set, if credentials set, check if there are updates from GitHub warning of changes.
}

