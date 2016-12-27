library(testthat)
library(eidith)


test_that("ed_tests_report is returning an error", {
  expect_error(ed_tests_report(), NA)
}
)


test_check("eidith")

