.eidith_db <- NULL
#' @importFrom rappdirs user_data_dir
default_sql_path <- function(){
  return(dplyr::if_else(
    file.access(rappdirs::user_data_dir(), 2) == 0, #if user has write permissions
    file.path(rappdirs::user_data_dir(),            #then write to eidith/eidith_db.sqlite
              "eidith", "eidith_db.sqlite"),
    file.path(                                      #else write to this file-path
      system.file(package = "eidith", mustWork = FALSE),
      "eidith", "eidith_db.sqlite")))
}

temp_sql_path <- function(){
  return(dplyr::if_else(
    file.access(rappdirs::user_data_dir(), 2) == 0, #if user has write permissions
    file.path(rappdirs::user_data_dir(),            #then write to eidith/eidith_db.sqlite
              "eidith", "temp_db.sqlite"),
    file.path(                                      #else write to this file-path
      system.file(package = "eidith", mustWork = FALSE),
      "eidith", "temp_db.sqlite")))
}

eidith_db <- function(path = NULL) {
  if(!is.null(path)) {
    return(dplyr::src_sqlite(path, create=TRUE))      #creates a new sqlite if there's a specific path
  } else {
    current_path <- normalizePath(getOption("ed_sql_path", default_sql_path()),
                                  mustWork=FALSE)
    if(!dir.exists(dirname(current_path))) {
      dir.create(dirname(current_path), recursive=TRUE)
    }
    if(is.null(.eidith_db) || .eidith_db$con@dbname != current_path) {
      .eidith_db <<- dplyr::src_sqlite(current_path, create=TRUE) #connect to database and create if it isn't there
    }
    return(.eidith_db)
  }
}

.onLoad <- function(libname, pkgname) {
  .eidith_db <<- eidith_db()
}

.onAttach <- function(libname, pkgname) {
  if(interactive())
    packageStartupMessage(ed_create_banner(), ed_db_status_msg(ed_db_make_details()))
    packageStartupMessage(ed_db_check_status())
  unlockBinding(".eidith_db", env=asNamespace("eidith"))       #allows .eidith_db to be edited
}

