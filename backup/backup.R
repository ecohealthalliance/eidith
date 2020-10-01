# This script was a last-minute backup for the test and test interp tables on 9/30/2020 as the eidith API was being taken down. The ed_download functions were failing due to very long json strings!!
# the event table is here just for testing purposes

library(eidith)
library(httr)
library(jsonlite)

auth <- eidith:::ed_auth()


event_url <- "https://predict2api.eidith.org/api/Extract/ExtractEventData?p1data=1"
test_url <- "https://predict2api.eidith.org/api/Extract/ExtractTestData?p1data=1&sequence=1"
test_interp_url <- "https://predict2api.eidith.org/api/Extract/ExtractTestDataInterpreted?p1data=1"

GET(url = event_url, authenticate(auth[1], auth[2], type="basic"), write_disk(here::here("backup/event.json")))
GET(url = test_url, authenticate(auth[1], auth[2], type="basic"), write_disk(here::here("backup/test.json")))
GET(url = test_interp_url, authenticate(auth[1], auth[2], type="basic"), write_disk(here::here("backup/TestInterpreted.json")))

event <- fromJSON(here::here("backup/event.json"))
event_clean <- eidith:::ed2_process(event, "Event")
event_clean$integer_id <- seq_len(nrow(event_clean))
event_exists <- ed2_events()
dim(event_exists)
DBI::dbWriteTable(eidith:::eidith_db(),
               value = event_clean,
               name = eidith:::p2_table_names[["Event"]],
               overwrite = TRUE)
event_new <- ed2_events()
dim(event_new)
setdiff(names(event_exists), names(event_new))


test <- fromJSON(here::here("backup/test.json"))
test_clean <- eidith:::ed2_process(test, "Test")
test_clean$integer_id <- seq_len(nrow(test_clean))
test_exists <- ed2_tests()
dim(test_exists)
unique(test_exists$project)
DBI::dbWriteTable(eidith:::eidith_db(),
                  value = test_clean,
                  name = eidith:::p2_table_names[["Test"]],
                  overwrite = TRUE)
test_new <- ed2_tests()
dim(test_new)
unique(test_new$project)
setdiff(names(test_exists), names(test_new))


test_interp <- fromJSON(here::here("backup/TestInterpreted.json"))
test_interp_clean <- eidith:::ed2_process(test_interp, "TestDataInterpreted")
test_interp_clean$integer_id <- seq_len(nrow(test_interp_clean))
test_interp_exists <- ed2_test_interpreted()
dim(test_interp_exists)
unique(test_interp_exists$project)
DBI::dbWriteTable(eidith:::eidith_db(),
                  value = test_interp_clean,
                  name = eidith:::p2_table_names[["TestDataInterpreted"]],
                  overwrite = TRUE)
test_interp_new <- ed2_test_interpreted()
dim(test_interp_new)
unique(test_interp_new$project)
setdiff(names(test_interp_exists), names(test_interp_new))

