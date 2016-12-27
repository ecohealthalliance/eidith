DB_EXISTS <- length(ed_db_status()) == 7

if(!DB_EXISTS) {
  message("Database empty, some tests will be skipped")
} else {

  test_that("ed_tests_report returns an error", {
    expect_error(ed_tests_report(), NA)
  })

}
