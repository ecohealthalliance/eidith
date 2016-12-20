DB_EXISTS <- length(ed_db_status()) == 7

context("Tables")

if(!DB_EXISTS) {
  message("Database empty, skipping data extract tests")
} else {

test_that("Table extracts work", {
  expect_identical(ed_events(event_date == "2010-07-12"),
                   dplyr::filter_(ed_events(), ~event_date=="2010-07-12"))
  expect_identical(ed_animals(sex == "Female"),
                   dplyr::filter_(ed_animals(), ~sex=="Female"))
})


}
