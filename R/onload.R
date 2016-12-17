.eidith_db <- NULL
default_sql_path <-  getOption("ed_sql_path",
                               file.path(rappdirs::user_data_dir(),
                                         "eidith", "eidith_db.sqlite"))


eidith_db <- function(path = NULL) {
  if(!is.null(path)) {
    return(dplyr::src_sqlite(path, create=TRUE))
  } else {
    current_path <- getOption("ed_sql_path", default_sql_path)
    if(!dir.exists(dirname(current_path))) {
      dir.create(dirname(current_path), recursive=TRUE)
    }
    if(is.null(.eidith_db) || .eidith_db$path != current_path) {
      .eidith_db <<- dplyr::src_sqlite(current_path, create=TRUE)
    }
    return(.eidith_db)
  }
}

.onLoad <- function(libname, pkgname) {
  .eidith_db <<- eidith_db()
}

.onAttach <- function(libname, pkgname) {
  if(interactive())
    packageStartupMessage(ed_db_status_msg(ed_db_status()))
}

