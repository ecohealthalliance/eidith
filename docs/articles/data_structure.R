## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
library(eidith)
library(tidyverse)
library(ggplot2)
source('~/R/eidith/R/tables_conn.R')
P <- rprojroot::find_rstudio_root_file()

## ----1-------------------------------------------------------------------
#  library(eidith)

## ----2-------------------------------------------------------------------
#  install.packages("tidyverse")
#  library(tidyverse)

## ----conn, eval = TRUE---------------------------------------------------
ed_tables_conn()

## ----3, eval = TRUE------------------------------------------------------
mock_events <- ed_events_mock()
mock_animals <- ed_animals_mock()
mock_specimens <- ed_specimens_mock()

## ----4, eval = TRUE------------------------------------------------------
mock_ea <- inner_join(mock_events, mock_animals, by = "event_id")

## ----5, eval = TRUE------------------------------------------------------
mock_eas <- inner_join(mock_ea, mock_specimens, by = "animal_id")

## ----6-------------------------------------------------------------------
#  install.packages("ggplot2")
#  library(ggplot2)

## ----7, eval = TRUE------------------------------------------------------
ggplot(data = mock_eas) +       
  geom_bar(aes(x = specimen_type_id, fill = habitat_type)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text("")) +
  labs(title = "Specimen Type by Habitat Type (Mock Data)", x = "Specimen Type", y = "Count (specimens)", fill = "Habitat Type")

