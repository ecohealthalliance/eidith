# this script filters the global database (from the global download) for just eha countries or just eha + malaysia

library(eidith)
library(tidyverse)
library(DBI)

import_local_db(database = "global")

# Initiate filtered dbs ------------------------------------------------------------
conn_eha_with_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_with_malaysia/eidith_db.sqlite"))
conn_eha_no_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_no_malaysia/eidith_db.sqlite"))

# Events + extracts -----------------------------------------------------------
#TODO update this to filter by event_id and project (using inner join)
tbls <- p2_api_endpoints()
event_extracts <- c("Event", "AnimalProduction", "CropProduction", "Dwellings",  "ExtractiveIndustry",
                    "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary" )

events <- ed2_events()

events_eha_with_malay <- events %>%
  filter(country %in% eha_countries())
events_eha_with_malay_names <- unique(events_eha_with_malay$event_name)

events_eha_no_malay <- events_eha_with_malay %>%
  filter(!country %in% c("Malaysia, Peninsular", "Malaysia, Sabah"))
events_eha_no_malay_names <- unique(events_eha_no_malay$event_name)

for(ee in event_extracts){

  # read full global table
  tb_name <- eidith:::p2_table_names[[ee]]
  tb <- DBI::dbReadTable(eidith:::eidith_db(), tb_name)

  # filter for eha with malaysia
  tb_eha_with_malay <- tb %>% filter(event_name %in% events_eha_with_malay_names)
  DBI::dbWriteTable(conn_eha_with_malay,
                    value = tb_eha_with_malay,
                    name = tb_name,
                    overwrite = TRUE)

  # filter for eha without malaysia
  tb_eha_no_malay <- tb %>% filter(event_name %in% events_eha_no_malay_names)
  DBI::dbWriteTable(conn_eha_no_malay,
                    value = tb_eha_no_malay,
                    name = tb_name,
                    overwrite = TRUE)
}

# lookups for other tables
lookup_events_eha_with_malay <- events_eha_with_malay %>%
  select(project, gains4_event_id) %>%
  distinct()

lookup_events_eha_no_malay <- events_eha_no_malay %>%
  select(project, gains4_event_id) %>%
  distinct()

# Animal ------------------------------------------------------------------

animals <- ed2_animals()

# join for eha with malaysia
animals_eha_with_malay <- inner_join(lookup_events_eha_with_malay, animals) %>%
  as.data.frame()

DBI::dbWriteTable(conn_eha_with_malay,
                  value = animals_eha_with_malay,
                  name =  eidith:::p2_table_names[["Animal"]],
                  overwrite = TRUE)

# join for eha no malaysia
animals_eha_no_malay <- inner_join(lookup_events_eha_no_malay, animals) %>%
  as.data.frame()

DBI::dbWriteTable(conn_eha_no_malay,
                  value = animals_eha_no_malay,
                  name =  eidith:::p2_table_names[["Animal"]],
                  overwrite = TRUE)

# Human + extracts -------------------------------------------------------------------

human_extracts <- p2_api_endpoints()[grep("Human", p2_api_endpoints())]

human <- ed2_human()
human_ehp <- ed2_human_ehp()

# get pid for eha with malaysia
lookup_events_eha_with_malay_p2 <- lookup_events_eha_with_malay %>%
  filter(project == "P2")
human_eha_with_malay <- human %>%
  filter(gains4_event_id %in% lookup_events_eha_with_malay_p2$gains4_event_id)
human_ehp_eha_with_malay <- human_ehp %>%
  filter(gains4_event_id %in% lookup_events_eha_with_malay_p2$gains4_event_id)
human_eha_with_malay_pid <- c(unique(human_eha_with_malay$participant_id), unique(human_ehp_eha_with_malay$participant_id))

# get pid for eha no malaysia
lookup_events_eha_no_malay_p2 <- lookup_events_eha_no_malay %>%
  filter(project == "P2")
human_eha_no_malay <- human %>%
  filter(gains4_event_id %in% lookup_events_eha_no_malay_p2$gains4_event_id)
human_ehp_eha_no_malay <- human_ehp %>%
  filter(gains4_event_id %in% lookup_events_eha_no_malay_p2$gains4_event_id)
human_eha_no_malay_pid <- c(unique(human_eha_no_malay$participant_id), unique(human_ehp_eha_no_malay$participant_id))


# filter and save human extracts
for(he in human_extracts){

  # read full global table
  tb_name <- eidith:::p2_table_names[[he]]
  tb <- DBI::dbReadTable(eidith:::eidith_db(), tb_name)

  # filter for eha with malaysia
  tb_eha_with_malay <- tb %>% filter(participant_id %in% human_eha_with_malay_pid)
  DBI::dbWriteTable(conn_eha_with_malay,
                    value = tb_eha_with_malay,
                    name = tb_name,
                    overwrite = TRUE)

  # filter for eha without malaysia
  tb_eha_no_malay <- tb %>% filter(participant_id %in% human_eha_no_malay_pid)
  DBI::dbWriteTable(conn_eha_no_malay,
                    value = tb_eha_no_malay,
                    name = tb_name,
                    overwrite = TRUE)
}

# Specimen ----------------------------------------------------------------
specimens <- ed2_specimens()

# lookup sample unit id for eha with malay
lookup_animals_eha_with_malay <- animals_eha_with_malay %>%
  select(project, gains4_sample_unit_id)
lookup_human_eha_with_malay <- human_eha_with_malay %>%
  select(gains4_sample_unit_id) %>%
  mutate(project = "P2")
lookup_human_ehp_eha_with_malay <- human_ehp_eha_with_malay %>%
  select(gains4_sample_unit_id) %>%
  mutate(project = "P2")
lookup_sui_with_malay <- bind_rows(lookup_animals_eha_with_malay, lookup_human_eha_with_malay) %>%
  bind_rows(lookup_human_ehp_eha_with_malay)

specimens_eha_with_malay <- inner_join(lookup_sui_with_malay, specimens)

DBI::dbWriteTable(conn_eha_with_malay,
                  value = specimens_eha_with_malay,
                  name =  eidith:::p2_table_names[["Specimen"]],
                  overwrite = TRUE)

# lookup sample unit id for eha no malay
lookup_animals_eha_no_malay <- animals_eha_no_malay %>%
  select(project, gains4_sample_unit_id)
lookup_human_eha_no_malay <- human_eha_no_malay %>%
  select(gains4_sample_unit_id) %>%
  mutate(project = "P2")
lookup_human_ehp_eha_no_malay <- human_ehp_eha_no_malay %>%
  select(gains4_sample_unit_id) %>%
  mutate(project = "P2")
lookup_sui_no_malay <- bind_rows(lookup_animals_eha_no_malay, lookup_human_eha_no_malay) %>%
  bind_rows(lookup_human_ehp_eha_no_malay)

specimens_eha_no_malay <- inner_join(lookup_sui_no_malay, specimens)

DBI::dbWriteTable(conn_eha_no_malay,
                  value = specimens_eha_no_malay,
                  name =  eidith:::p2_table_names[["Specimen"]],
                  overwrite = TRUE)

# Test --------------------------------------------------------------------
test <- ed2_tests()

# lookup specimen id for eha with malay
lookup_specimens_eha_with_malay <- specimens_eha_with_malay %>%
  select(project, gains4_specimen_id)
test_eha_with_malay <- inner_join(lookup_specimens_eha_with_malay, test)

DBI::dbWriteTable(conn_eha_with_malay,
                  value = test_eha_with_malay,
                  name =  eidith:::p2_table_names[["Test"]],
                  overwrite = TRUE)

# lookup specimen id for eha no malay
lookup_specimens_eha_no_malay <- specimens_eha_no_malay %>%
  select(project, gains4_specimen_id)
test_eha_no_malay <- inner_join(lookup_specimens_eha_no_malay, test)

DBI::dbWriteTable(conn_eha_no_malay,
                  value = test_eha_no_malay,
                  name =  eidith:::p2_table_names[["Test"]],
                  overwrite = TRUE)
# Behavioral --------------------------------------------------------------
behavior <- ed2_behavior()

behavior_eha <- behavior %>%
  filter(country %in% eha_countries())

unique(behavior_eha$country)

DBI::dbWriteTable(conn_eha_with_malay,
                  value = behavior_eha,
                  name =  eidith:::p2_table_names[["Behavioral"]],
                  overwrite = TRUE)

DBI::dbWriteTable(conn_eha_with_malay,
                  value = behavior_eha,
                  name =  eidith:::p2_table_names[["Behavioral"]],
                  overwrite = TRUE)


# Training ----------------------------------------------------------------

training <- ed2_training()
training_eha_with_malay <- training %>%
  filter(participant_home_country %in% eha_countries())
DBI::dbWriteTable(conn_eha_with_malay,
                  value = as.data.frame(training_eha_with_malay),
                  name = "training_2",
                  overwrite = TRUE)

training_eha_no_malay <- training_eha_with_malay %>%
  filter(!participant_home_country %in% c("Malaysia, Peninsular", "Malaysia, Sabah"))
DBI::dbWriteTable(conn_eha_no_malay,
                  value = as.data.frame(training_eha_no_malay),
                  name = "training_2",
                  overwrite = TRUE)

dbListTables(conn_eha_with_malay)
dbListTables(conn_eha_no_malay)

dbDisconnect(conn_eha_with_malay)
dbDisconnect(conn_eha_no_malay)

