HAS_INTERNET <- curl::has_internet()
HAS_GLOBAL_CRED <- (Sys.getenv("EIDITH_GLOBAL_USERNAME") != "") &&
  (Sys.getenv("EIDITH_GLOBAL_PASSWORD") != "")
HAS_EHA_CRED <- (Sys.getenv("EIDITH_EHA_USERNAME") != "") &&
  (Sys.getenv("EIDITH_EHA_PASSWORD") != "")

if(!HAS_INTERNET) warning("No internet connection, download tests will be skipped")
if(!HAS_GLOBAL_CRED) warning("No global credentials, some tests will be skipped")
if(!HAS_EHA_CRED) warning("No EHA credentials, some tests will be skipped")

