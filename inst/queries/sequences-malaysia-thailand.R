library(eidith)
library(tidyverse)
library(DBI)
h <- here::here


tests <- ed2_tests() %>%
  filter(country %in% c("Malaysia, Sabah",  "Malaysia, Peninsular", "Thailand")) %>%
  filter(test_result == "Product for Sequencing") %>%
  filter(confirmation_result == "Positive") %>%
  filter(test_requested %in% c("Coronaviruses",  "Paramyxoviruses", "Filoviruses"))

animals <- ed2_animals() %>%
  filter(country %in% c("Malaysia, Sabah",  "Malaysia, Peninsular", "Thailand")) %>%
  select(project, country, gains4_sample_unit_id, taxa_group, class, order, family, genus, species)


specimens <- ed2_specimens() %>%
  filter(country %in% c("Malaysia, Sabah",  "Malaysia, Peninsular", "Thailand")) %>%
  select(project, country, gains4_specimen_id, gains4_sample_unit_id, specimen_type_group)

out <- left_join(tests, specimens, by = c("project", "country", "gains4_specimen_id")) %>%
  left_join(animals, by = c("project", "country", "gains4_sample_unit_id"))


write_csv(out, here::here("inst","queries","malaysia-thailand-sequences.csv"))
