# this script fixes an issue in the original backup (01_backup.R). We were missing P1 from the animal table.
# Tammie provided us a backup of the animal table as a txt file. Here we add it to the db.
# We also remove integer_id field from all of the tables

library(tidyverse)
library(eidith)

import_local_db(database = "global")

# read in animal table from Tammie (email to ESM 10/30/2020)
animal <- read_delim(here::here("inst/backup/AnimalData_20200929.txt"), delim = "\t",
                     col_types = cols(.default = "c"))

# clean table names
animal_metadata <- ed2_metadata() %>%
  filter(endpoint2 == "Animal") %>%
  select(original_name, auto_processed_name, replacement_name)

animal_drop <- animal_metadata %>%
  filter(replacement_name == "DROP") %>%
  pull(original_name)

animal_keep <- animal_metadata %>%
  filter(!original_name %in% animal_drop)  %>%
  mutate(replacement_name = ifelse(is.na(replacement_name), auto_processed_name, replacement_name)) %>%
  select(-auto_processed_name)

animal <- animal %>%
  select(all_of(animal_keep$original_name))

all(names(animal) == animal_keep$original_name)
colnames(animal) <- animal_keep$replacement_name

# save table to db
DBI::dbWriteTable(eidith:::eidith_db(),
                  value = animal,
                  name = eidith:::p2_table_names[["Animal"]],
                  overwrite = TRUE)

# for all tables, remove integer id
for(tb in eidith:::p2_table_names){
  etb <- tbl(eidith:::eidith_db(), tb) %>%
    select(-one_of("integer_id")) %>%
    collect()

  DBI::dbWriteTable(eidith:::eidith_db(),
                    value = etb,
                    name = tb,
                    overwrite = TRUE)
}

# for all tables, make everything character
for(tb in eidith:::p2_table_names){
  etb <- tbl(eidith:::eidith_db(), tb) %>%
    collect() %>%
    mutate_all(as.character)

  DBI::dbWriteTable(eidith:::eidith_db(),
                    value = etb,
                    name = tb,
                    overwrite = TRUE)
}


DBI::dbDisconnect(eidith:::eidith_db())
