
withr::with_options(list(ed_sql_path = TEST_SQL_PATH), {

  DB_EXISTS <- length(ed_db_status()) == 7

  if(!DB_EXISTS) {
    message("Database empty, some tests will be skipped")
  } else {

    context("Tables")

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

    test_that("Tables can use %in% with zero-length vectors", {
      x = integer(0)
      y = character(0)
      expect_error(ed_animals(sex %in% x), NA)
      expect_error(ed_animals(sex %in% y), NA)
      expect_error(ed_animals(sex %in% c()), NA)
      expect_error(ed_animals(sex %in% NULL), NA)
    })

    test_that("Tables can use %in% with  one-length vectors", {
      expect_identical(ed_animals(sex %in% "Female"), ed_animals(sex == "Female"))
      x = "Female"
      y = c("Female", "Male")
      expect_error(ed_animals(sex %in% x), NA)
      expect_error(ed_animals(sex %in% y), NA)
      expect_error(ed_animals(sex %in% ("Female")), NA)
      expect_error(ed_animals(sex %in% c("Female")), NA)
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
})


