
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

}

