
options(ed_sql_path = normalizePath(file.path(Sys.getenv("HOME"), ".test_ed_db.sqlite"), mustWork=FALSE))

HAS_INTERNET <- curl::has_internet()
HAS_GLOBAL_CRED <- (Sys.getenv("EIDITH_GLOBAL_USERNAME") != "") &&
  (Sys.getenv("EIDITH_GLOBAL_PASSWORD") != "")
HAS_EHA_CRED <- (Sys.getenv("EIDITH_EHA_USERNAME") != "") &&
  (Sys.getenv("EIDITH_EHA_PASSWORD") != "")

if(!HAS_INTERNET) message("No internet connection, download tests will be skipped")
if(!HAS_GLOBAL_CRED) message("No global credentials, some tests will be skipped")
if(!HAS_EHA_CRED) message("No EHA credentials, some tests will be skipped")

