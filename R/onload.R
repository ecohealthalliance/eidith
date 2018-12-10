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
  db <- mget("db", envir = .eidith_env, ifnotfound = NA)[[1]]

  if (!is.null(path)) {
    if (inherits(db, "DBIConnection")) {
      if (DBI::dbIsValid(db) && normalizePath(path, mustWork = FALSE) == normalizePath(db@dbname, mustWork = FALSE)) {
        return(db)
      }
      db <- DBI::dbConnect(RSQLite::SQLite(), path)
      return(db)
    }
  } else {
    current_path <- normalizePath(getOption("ed_sql_path", default_sql_path()),
                                  mustWork = FALSE)
    if (inherits(db, "DBIConnection")) {
      if (DBI::dbIsValid(db) && normalizePath(current_path, mustWork = FALSE) == normalizePath(db@dbname, mustWork = FALSE)) {
        return(db)
      }
    } else {
      if (!dir.exists(dirname(current_path))) {
        dir.create(dirname(current_path), recursive = TRUE)
      }
      db <- DBI::dbConnect(RSQLite::SQLite(), current_path)
      assign("db", db, envir = .eidith_env)
      return(db)
    }
  }
}


eidith_disconnect <- function(.eidith_env) {
  db <- mget("db", envir = .eidith_env, ifnotfound = NA)[[1]]
  if (inherits(db, "DBIConnection")) {
    DBI::dbDisconnect(db)
  }
}

.eidith_env <- new.env(parent = emptyenv())
.eidith_env$db <- NULL
reg.finalizer(.eidith_env, eidith_disconnect, onexit = TRUE)

.onLoad <- function(libname, pkgname) {
  unlockBinding(".eidith_env", env=asNamespace("eidith"))       #allows .eidith_env to be edited
  invisible(eidith_db())
}

.onAttach <- function(libname, pkgname) {
  unlockBinding(".eidith_env", env=asNamespace("eidith"))       #allows .eidith_env to be edited
  if(interactive()){
    packageStartupMessage(crayon::black(ed_db_presence()))
    packageStartupMessage(ed_db_status_msg(ed_db_make_status_msg()))
    packageStartupMessage(ed_db_check_status(path = NULL, inter = FALSE))
  }
}

