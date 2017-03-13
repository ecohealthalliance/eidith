
suppressPackageStartupMessages({
  library(httr)
  library(dplyr)
  library(magrittr)
})

context("Web API")

if(HAS_INTERNET && HAS_EHA_CRED) {

  withr::with_options(list(ed_sql_path = TEST_SQL_PATH), {

    test_that("Server API has expected fields", {
      for(endpt in eidith:::endpoints[1:5]) {
        url <- modify_url(paste0(eidith:::eidith_base_url, endpt),
                          query = list(header_only="y"))
        headers = GET(url, authenticate(Sys.getenv("EIDITH_EHA_USERNAME"), Sys.getenv("EIDITH_EHA_PASSWORD"))) %>%
          content() %>%
          unlist()
        expected_fields <- filter(ed_metadata(), endpoint==endpt) %>% `$`(original_name) %>% na.omit
        expect_true(all(headers %in% expected_fields))
        expect_true(all(expected_fields %in% headers))
      }
    })

    test_that("Database download takes place", {

      withr::with_envvar(c("EIDITH_USERNAME"=Sys.getenv("EIDITH_EHA_USERNAME"),
                           "EIDITH_PASSWORD"=Sys.getenv("EIDITH_EHA_PASSWORD")), {
                             cat("\n")
                             expect_equal(ed_db_download(verbose = TRUE), 0)
                           })
    })
  })
}

