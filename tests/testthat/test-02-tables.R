DB_EXISTS <- length(ed_db_status()) == 7

context("Tables")

if(!DB_EXISTS) {
  message("Database empty, some tests")
} else {

  test_that("No fails on zero-length tables", {
    expect_error(ed_specimens(date_created > "2018-03-04"), regexp = NA)
  })

  test_that("Table extracts work", {

    expect_equivalent(ed_events(deep_forest_data==0),
                     dplyr::filter_(ed_events(), ~deep_forest_data==FALSE))

    expect_equivalent(ed_animals(sex == "Female"),
                     dplyr::filter_(ed_animals(), ~sex=="Female"))

    expect_equivalent(ed_specimens(date_created > "2018-03-04"),
                     dplyr::filter_(ed_specimens(), ~date_created > "2018-03-04"))
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

options(ed_sql_path=NULL)

unlink(normalizePath(file.path(Sys.getenv("HOME"), ".test_ed_db.sqlite"))
