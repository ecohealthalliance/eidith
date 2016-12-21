
suppressPackageStartupMessages({
  library(httr)
  library(dplyr)
  library(magrittr)
})

context("Web API")

if(HAS_INTERNET && HAS_GLOBAL_CRED) {

  test_that("Server API has expected fields", {
    for(endpt in eidith:::endpoints[1:5]) {
      url <- modify_url(paste0(eidith:::eidith_base_url, endpt),
                        query = list(header_only="y"))
      headers = GET(url, authenticate(Sys.getenv("EIDITH_GLOBAL_USERNAME"), Sys.getenv("EIDITH_GLOBAL_PASSWORD"))) %>%
        content() %>%
        unlist()
      expected_fields <- filter(ed_metadata(), endpoint==endpt) %>% use_series(original_name) %>% na.omit
      expect_true(all(headers %in% expected_fields))
      expect_true(all(expected_fields %in% headers))
    }
  })

  test_that("Database download takes place", {

    withr::with_envvar(c("EIDITH_USERNAME"=Sys.getenv("EIDITH_GLOBAL_USERNAME"),
                         "EIDITH_PASSWORD"=Sys.getenv("EIDITH_GLOBAL_PASSWORD")), {
                           cat("\n")
                           expect_equal(ed_db_download(verbose = TRUE), 0)
                         })
  })

}

