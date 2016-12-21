DB_EXISTS <- length(ed_db_status()) == 7

context("Tables")

if(!DB_EXISTS) {
  message("Database empty, some tests")
} else {

  test_that("Table extracts work", {
    expect_identical(ed_events(event_date == "2010-07-12"),
                     dplyr::filter_(ed_events(), ~event_date=="2010-07-12"))
    expect_identical(ed_animals(sex == "Female"),
                     dplyr::filter_(ed_animals(), ~sex=="Female"))
  })

  test_that("Fields in database are as expected", {
    for(tb in eidith:::db_tables[1:6]) {
      ed_tb <- tbl(eidith_db(), tb) %>% head %>% collect
      expected_fields <- filter(ed_metadata(), table == tb) %>%
        mutate(nname = coalesce(replacement_name, auto_processed_name)) %>%
        use_series(nname) %>%
        na.omit() %>%
        stringi::stri_subset_fixed("DROP", negate=TRUE)
      expect_true(all(names(ed_tb %in% expected_fields)))
      expect_true(all(names(expected_fields %in% ed_tb)))
    }
  })
}


unlink(getOption("ed_sql_path"))
option(ed_eql_path=NULL)
