library(eidith)
library(tidyverse)
library(DBI)

import_local_db(database = "global")
#TODO 1) databases 2) fix animals? 3) readme
#readme - import_local_db(),it will ask to access your google drive account
#password, api token for google


# Initiate filtered dbs ------------------------------------------------------------
conn_eha_with_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_with_malaysia/eidith_db.sqlite"))
conn_eha_no_malay <- DBI::dbConnect(RSQLite::SQLite(), here::here("inst/backup/eha_no_malaysia/eidith_db.sqlite"))

# Events + extracts -----------------------------------------------------------
tbls <- p2_api_endpoints()
event_extracts <- c("Event", "AnimalProduction", "CropProduction", "Dwellings",  "ExtractiveIndustry",
                    "MarketValueChain", "NaturalAreas", "WildlifeRestaurant", "ZooSanctuary" )

events_eha_with_malay <- ed2_events() %>%
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

# Animal ------------------------------------------------------------------

# missing p1!!

events_lookup <- ed2_events() %>%
  select(project, country, event_name) %>%
  distinct()

animal_lookup <- ed2_animals() %>%
  select(project, event_name) %>%
  distinct()
unique(animal_lookup$project) #! this should include P1

animal_event_lookup <- left_join(animal_lookup, events_lookup, by = "event_name", suffix = c(".animal", ".event"))
distinct(animal_event_lookup, project.animal, project.event)
# ^ inconsistency in project.event and project.animal
dim(animal_event_lookup)

events_lookup <- ed2_events() %>%
  select(project, country, gains4_event_id) %>%
  distinct()

animal_lookup <- ed2_animals() %>%
  select(project, event_name, gains4_event_id) %>%
  distinct()
unique(animal_lookup$project) #! this should include P1

animal_event_lookup2 <- left_join(animal_lookup, events_lookup, by = "gains4_event_id", suffix = c(".animal", ".event"))
distinct(animal_event_lookup2, project.animal, project.event)
# ^ inconsistency in project.event and project.animal
dim(animal_event_lookup2)

# Human + extracts -------------------------------------------------------------------

human_extracts <- p2_api_endpoints()[grep("Human", p2_api_endpoints())]

human <- ed2_human()
human_ehp <- ed2_human_ehp()

human_eha_with_malay <- human %>%
  filter(event_name %in% events_eha_with_malay_names)
human_ehp_eha_with_malay <- human_ehp %>%
  filter(event_name %in% events_eha_with_malay_names)
human_eha_with_malay_pid <- c(unique(human_eha_with_malay$participant_id), unique(human_ehp_eha_with_malay$participant_id))

human_eha_no_malay <- human %>%
  filter(event_name %in% events_eha_no_malay_names)
human_ehp_eha_no_malay <- human_ehp %>%
  filter(event_name %in% events_eha_no_malay_names)
human_eha_no_malay_pid <- c(unique(human_eha_no_malay$participant_id), unique(human_ehp_eha_no_malay$participant_id))

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

# Test --------------------------------------------------------------------


# Behavioral --------------------------------------------------------------
behavior <- ed2_behavior()
#???


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

