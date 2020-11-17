# this script filters the global database for just eha countries or just eha + malaysia
# it also adds the country column to each table
# it takes as input the database as exported from 02_fixing_animal_table.R (eidith/original)

library(eidith)
library(tidyverse)
library(DBI)

# Get original global db from google drive ---------------------------------------
drive_url <- "https://drive.google.com/file/d/1noDn8YoGS-fyKgqGOkgi2bOkxirDGXIB/view?usp=sharing"
local_path <- file.path(rappdirs::user_data_dir(),
                        "eidith")
local_path_zip <- paste0(local_path,  "/eidith_db.sqlite.zip")
googledrive::drive_download(file = drive_url,
                            path = local_path_zip,
                            overwrite = TRUE)
unzip(zipfile = local_path_zip, exdir = local_path)

# Initiate dbs  ------------------------------------------------------------
conn_global <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/global/eidith_db.sqlite"))
conn_eha_with_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_with_malaysia/eidith_db.sqlite"))
conn_eha_no_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_no_malaysia/eidith_db.sqlite"))

# Function to save to dbs --------------------------------------------------
save_tb_to_dbs <- function(tb, tb_name){
  DBI::dbWriteTable(conn_global,
                    value = tb,
                    name = tb_name,
                    overwrite = TRUE)
  DBI::dbWriteTable(conn_eha_with_malay,
                    value = tb %>% filter(country %in% eha_countries()),
                    name = tb_name,
                    overwrite = TRUE)
  DBI::dbWriteTable(conn_eha_no_malay,
                    value = tb %>% filter(country %in% eha_countries(), !country %in% c("Malaysia, Peninsular", "Malaysia, Sabah" )),
                    name = tb_name,
                    overwrite = TRUE)
}

# events -----------------------------------------------------------
events <- ed2_events()
save_tb_to_dbs(tb = events, tb_name =  eidith:::p2_table_names[["Event"]])
lookup_events <- events %>%
  select(project, gains4_event_id, event_name, country) %>%
  distinct()

# event extracts -----------------------------------------------------------
## join with events by event_name
event_extracts <- c("AnimalProduction", "CropProduction", "Dwellings",  "ExtractiveIndustry",
                    "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary" )

for(ee in event_extracts){
  tb_name <- eidith:::p2_table_names[[ee]]
  tb <- DBI::dbReadTable(eidith:::eidith_db(), tb_name)
  assertthat::assert_that(all(unique(tb$event_name) %in% unique(lookup_events$event_name)))
  tb2 <- inner_join(lookup_events, tb, by = "event_name") # this may expand in size relative to tb because some events have multiple gain_4 ids
  assertthat::assert_that(!any(str_detect(colnames(tb2), "\\.x|\\.y")))
  assertthat::assert_that(nrow(tb) == nrow(tb2))
  save_tb_to_dbs(tb = tb2, tb_name = tb_name)
}

# animals -----------------------------------------------------------------
## join with events by gains4_event_id
animals <- ed2_animals() %>%
  select(-event_name) # let's use event_name from events table since there are some minor inconsistencies that cause data to be lost in the join
assertthat::assert_that(all(unique(animals$gains4_event_id) %in% unique(lookup_events$gains4_event_id)))
animals2 <- inner_join(lookup_events, animals, by = c("project", "gains4_event_id"))
assertthat::assert_that(!any(str_detect(colnames(animals2), "\\.x|\\.y")))
assertthat::assert_that(nrow(animals) == nrow(animals2))
save_tb_to_dbs(tb = animals2, tb_name =  eidith:::p2_table_names[["Animal"]])
lookup_animals <- animals2  %>%
  select(project, country, gains4_sample_unit_id)

# humans ------------------------------------------------------------------
## join with events by gains4_event_id
human <- ed2_human() %>%
  mutate(project = "P2") %>%
  select(-event_name) # let's use event_name from events table since there are some minor inconsistencies that cause data to be lost in the join
assertthat::assert_that(all(unique(human$gains4_event_id) %in% unique(lookup_events$gains4_event_id)))
human2 <- inner_join(lookup_events, human, by = c("project", "gains4_event_id"))
assertthat::assert_that(!any(str_detect(colnames(human2), "\\.x|\\.y")))
assertthat::assert_that(nrow(human) == nrow(human2))
save_tb_to_dbs(tb = human2, tb_name =  eidith:::p2_table_names[["Human"]])
lookup_human <- human2  %>%
  select(project, country, gains4_sample_unit_id)

# human ehp ---------------------------------------------------------------
## join with events by gains4_event_id
human_ehp <- ed2_human_ehp() %>%
  mutate(project = "P2") %>%
  select(-event_name) # let's use event_name from events table since there are some minor inconsistencies that cause data to be lost in the join
assertthat::assert_that(all(unique(human_ehp$gains4_event_id) %in% unique(lookup_events$gains4_event_id)))
human_ehp2 <- inner_join(lookup_events, human_ehp, by = c("project", "gains4_event_id"))
assertthat::assert_that(!any(str_detect(colnames(human_ehp2), "\\.x|\\.y")))
assertthat::assert_that(nrow(human_ehp) == nrow(human_ehp2))
save_tb_to_dbs(tb = human_ehp2, tb_name =  eidith:::p2_table_names[["HumanEHP"]])
lookup_human_ehp <- human_ehp2  %>%
  select(project, country, gains4_sample_unit_id)
lookup_human <- bind_rows(lookup_human, lookup_human_ehp)

# human extracts ----------------------------------------------------------
## join with humans by gains4_sample_unit_id
human_extracts <- p2_api_endpoints()[grep("Human", p2_api_endpoints())]
human_extracts <- human_extracts[!human_extracts %in% c("Human", "HumanEHP")]

for(he in human_extracts){
  tb_name <- eidith:::p2_table_names[[he]]
  tb <- DBI::dbReadTable(eidith:::eidith_db(), tb_name) %>%
    select(-one_of("event_name"))
  assertthat::assert_that(all(unique(tb$gains4_sample_unit_id) %in% unique(lookup_human$gains4_sample_unit_id)))
  tb2 <- inner_join(lookup_human, tb, by = "gains4_sample_unit_id") # this may expand in size relative to tb because some events have multiple gain_4 ids
  assertthat::assert_that(!any(str_detect(colnames(tb2), "\\.x|\\.y")))
  assertthat::assert_that(nrow(tb) == nrow(tb2))
  save_tb_to_dbs(tb = tb2, tb_name = tb_name)
}

# specimen ----------------------------------------------------------------
## join with humans + animals by gains4_sample_unit_id
lookup_sui <- bind_rows(lookup_animals, lookup_human)
specimens <- ed2_specimens()
assertthat::assert_that(all(unique(specimens$gains4_sample_unit_id) %in% unique(lookup_sui$gains4_sample_unit_id)))
specimens2 <- inner_join(lookup_sui, specimens, by = c("project", "gains4_sample_unit_id"))
assertthat::assert_that(!any(str_detect(colnames(specimens2), "\\.x|\\.y")))
assertthat::assert_that(nrow(specimens) == nrow(specimens2))
save_tb_to_dbs(tb = specimens2, tb_name =  eidith:::p2_table_names[["Specimen"]])
lookup_specimen <- specimens2 %>%
  select(project, country, gains4_specimen_id)

# test --------------------------------------------------------------------
## join with specimen by gains4_specimen_id
test <- ed2_tests()
assertthat::assert_that(all(unique(test$gains4_specimen_id) %in% unique(lookup_specimen$gains4_specimen_id)))
# ---
# there are some specimen IDs in test but not specimen
# eg gains4_specimen_id "1015696" belongs to animal_id "LAP11-F0061" ie gains_4_sample_unit_id "34566". This sample unit has two gains4_specimen_id (201929, 1015696)
# most of these cases are the malaysia pooled samples that are okay to drop
# diff <- setdiff(unique(test$gains4_specimen_id), unique(lookup_specimen$gains4_specimen_id))
# length(diff)
# test_no_match <- test %>% filter(gains4_specimen_id %in% diff)
# test_no_match_p2 <- test_no_match %>% filter(project == "P2")
# ---
test2 <- inner_join(lookup_specimen, test, by = c("project", "gains4_specimen_id"))
assertthat::assert_that(!any(str_detect(colnames(test2), "\\.x|\\.y")))
assertthat::assert_that(nrow(test) == nrow(test2)) # error expected b/c above issues
save_tb_to_dbs(tb = test2, tb_name =  eidith:::p2_table_names[["Test"]])

# test interpreted --------------------------------------------------------------------
## join with specimen by gains4_specimen_id
test_interpreted <- ed2_test_interpreted()
assertthat::assert_that(all(unique(test_interpreted$gains4_specimen_id) %in% unique(lookup_specimen$gains4_specimen_id)))
# ---
# there are some specimen IDs in test interp but not specimen
# most of these cases are the malaysia pooled samples that I think are okay to drop
# diff <- setdiff(unique(test_interpreted$gains4_specimen_id), unique(lookup_specimen$gains4_specimen_id))
# length(diff)
# test_interp_no_match <- test_interpreted %>% filter(gains4_specimen_id %in% diff)
# test_interp_match_p2 <- test_interp_no_match %>% filter(project == "P2")
# unique(test_interp_match_p2$lab_name)
# ---
test_interpreted2 <- inner_join(lookup_specimen, test_interpreted, by = c("project", "gains4_specimen_id"))
assertthat::assert_that(!any(str_detect(colnames(test_interpreted2), "\\.x|\\.y")))
assertthat::assert_that(nrow(test_interpreted) == nrow(test_interpreted2)) # error expected b/c above issues
save_tb_to_dbs(tb = test_interpreted2, tb_name =  eidith:::p2_table_names[["TestDataInterpreted"]])

# test serology --------------------------------------------------------------------
## join with specimen by gains4_specimen_id
test_serology <- ed2_test_serology() %>%
  mutate(project = "P2")
assertthat::assert_that(all(unique(test_serology$gains4_specimen_id) %in% unique(lookup_specimen$gains4_specimen_id)))
test_serology2 <- inner_join(lookup_specimen, test_serology, by = c("project", "gains4_specimen_id"))
assertthat::assert_that(!any(str_detect(colnames(test_serology2), "\\.x|\\.y")))
assertthat::assert_that(nrow(test_serology) == nrow(test_serology2))
save_tb_to_dbs(tb = test_serology2, tb_name =  eidith:::p2_table_names[["TestDataSerology"]])

# behavior ----------------------------------------------------------------
behavior <- ed2_behavior()
save_tb_to_dbs(tb = behavior, tb_name =  eidith:::p2_table_names[["Behavioral"]])

# training ----------------------------------------------------------------
training <- ed2_training()
DBI::dbWriteTable(conn_global,
                  value = training,
                  name = eidith:::p2_table_names[["Training"]],
                  overwrite = TRUE)
DBI::dbWriteTable(conn_eha_with_malay,
                  value = training %>% filter(participant_home_country %in% eha_countries()),
                  name = eidith:::p2_table_names[["Training"]],
                  overwrite = TRUE)
DBI::dbWriteTable(conn_eha_no_malay,
                  value = training %>% filter(participant_home_country %in% eha_countries(), !participant_home_country %in% c("Malaysia, Peninsular", "Malaysia, Sabah" )),
                  name = eidith:::p2_table_names[["Training"]],
                  overwrite = TRUE)

# check and close ---------------------------------------------------------
dbListTables(conn_global)
dbListTables(conn_eha_with_malay)
dbListTables(conn_eha_no_malay)

dbDisconnect(conn_global)
dbDisconnect(conn_eha_with_malay)
dbDisconnect(conn_eha_no_malay)

