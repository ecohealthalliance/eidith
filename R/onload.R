eidith_db <- NULL # need to pre-create this  so we can assign to it in .onLoad

.onLoad <- function(libname, pkgname) {
  eidith_db_path <- file.path(rappdirs::user_data_dir(), "eidith")
  eidith_db_file <-
    if (!dir.exists(eidith_db_path)) dir.create(eidith_db_path)
  eidith_db <<- DBI::dbConnect(RSQLite::SQLite(), #assigning to the pkg env
                                file.path(eidith_db_path, "eidith_db.sqlite"))
  eidith_db <<- dplyr::src_sqlite(file.path(eidith_db_path, "eidith_db.sqlite"),
                                  create=TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Some message about db status") #Describe current countries in db, update set, if credentials set, check if there are updates from GitHub warning of changes.
}

download_db <- function() {
  auth <- eidith_auth()
  withr::with_envvar(c(EIDITH_USERNAME=auth[1], EIDITH_PASSWORD=auth[2]), {
    tables <- lapply(endpoints, ed_get, postprocess=FALSE)
  })
  lapply(db_list_tables(eidith_db$con), function(x) db_drop_table(eidith_db$con, x))
  lapply(seq_along(tables), function(x) {
    dplyr::copy_to(eidith_db, tables[[x]], name=db_tables[x], temporary = FALSE,
                   unique_indexes = db_indexes[x], unique=FALSE)
  })

}

update_db <- function() {

}

export_db <- function() {  #Exports the database file to new location.  options(eidith_db) should let you change it.

}

#' Retrieve credentials from cmdline or environment
# Use getPass()
eidith_creds <- function() {
  creds <- list(username=NULL, password=NULL)
  invisible(creds)
}

